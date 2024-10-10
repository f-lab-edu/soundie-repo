package com.soundie.post.service;

import com.soundie.global.common.util.CacheNames;
import com.soundie.post.domain.PostLike;
import com.soundie.post.repository.PostLikeRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

@Slf4j
@Service
@RequiredArgsConstructor
public class PostLikeScheduler {

    private final Map<Long, Set<Long>> saveRequests = new ConcurrentHashMap<>();
    private final Map<Long, Set<Long>> deleteRequests = new ConcurrentHashMap<>();
    private final PostLikeRepository postLikeRepository;
    private final RedisTemplate<String, Object> redisCacheTemplate;

    @Async("threadPoolTaskScheduler")
    @Scheduled(fixedDelay = 1000, initialDelay = 1000)
    public void schedulePostLikeUpdate() {
        // 좋아요 저장 요청
        saveRequests.forEach((postId, memberSet) -> {
            // db 초기화
            List<PostLike> saveLikes = memberSet.stream()
                    .map(memberId -> new PostLike(memberId, postId))
                    .toList();
            postLikeRepository.saveAll(saveLikes);

            // 캐시 존재 판단에 따른, 캐시 초기화
            if (Boolean.TRUE.equals(redisCacheTemplate.hasKey(getLikeCountKeyByPost(postId)))){
                ValueOperations<String, Object> opsForValue = redisCacheTemplate.opsForValue();
                opsForValue.increment(getLikeCountKeyByPost(postId), saveLikes.size());
            }
        });
        saveRequests.clear();

        // 좋아요 삭제 요청
        deleteRequests.forEach((postId, memberSet) -> {
            // db 초기화
            List<PostLike> deleteLikes = memberSet.stream()
                    .map(memberId -> new PostLike(memberId, postId))
                    .toList();
            postLikeRepository.deleteAll(deleteLikes);

            // 캐시 존재 판단에 따른, 캐시 초기화
            if (Boolean.TRUE.equals(redisCacheTemplate.hasKey(getLikeCountKeyByPost(postId)))){
                ValueOperations<String, Object> opsForValue = redisCacheTemplate.opsForValue();
                opsForValue.decrement(getLikeCountKeyByPost(postId), deleteLikes.size());
            }
        });
        deleteRequests.clear();
    }

    public void saveLike(Long postId, Long memberId) {
        // 첫 좋아요 저장
        if (!saveRequests.containsKey(postId)) {
            saveRequests.put(postId, Collections.synchronizedSet(new HashSet<>()));
            saveRequests.get(postId).add(memberId);
            return;
        }

        // 이미 좋아요 저장
        if (saveRequests.get(postId).contains(memberId)) {
            saveRequests.get(postId).remove(memberId);
            if (saveRequests.get(postId).isEmpty()) {
                saveRequests.remove(postId);
            }
            return;
        }

        saveRequests.get(postId).add(memberId);
    }

    public void deleteLike(Long postId, Long memberId) {
        // 첫 좋아요 삭제
        if (!deleteRequests.containsKey(postId)) {
            deleteRequests.put(postId, Collections.synchronizedSet(new HashSet<>()));
            deleteRequests.get(postId).add(memberId);
            return;
        }

        // 이미 좋아요 삭제
        if (deleteRequests.get(postId).contains(memberId)) {
            deleteRequests.get(postId).remove(memberId);
            if (deleteRequests.get(postId).isEmpty()) {
                deleteRequests.remove(postId);
            }
            return;
        }

        deleteRequests.get(postId).add(memberId);
    }

    private String getLikeCountKeyByPost(Long postId) {
        return CacheNames.LIKE_COUNT + "::"
                + "postId_" + postId;
    }
}
