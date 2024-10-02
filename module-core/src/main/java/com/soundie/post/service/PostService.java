package com.soundie.post.service;

import com.soundie.comment.repository.CommentRepository;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.BadRequestException;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.global.common.util.CacheExpireTime;
import com.soundie.global.common.util.CacheNames;
import com.soundie.global.common.util.PaginationUtil;
import com.soundie.image.service.ImageService;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.domain.PostLike;
import com.soundie.post.domain.PostWithCount;
import com.soundie.post.dto.*;
import com.soundie.post.repository.PostLikeRepository;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PostService {

    private final PostRepository postRepository;
    private final PostLikeRepository postLikeRepository;
    private final MemberRepository memberRepository;
    private final CommentRepository commentRepository;
    private final ImageService imageService;

    private final RedisTemplate<String, Object> redisCacheTemplate;
    private final ThreadPoolTaskExecutor threadPoolTaskExecutor;

    public GetPostResDto readPostList(){
        List<Post> findPosts = postRepository.findPosts();
        List<PostWithCount> findPostsWithCount = findPostsWithCount(findPosts);

        return GetPostResDto.of(findPostsWithCount);
    }

    public GetPostCursorResDto readPostListByCursor(GetPostCursorReqDto getPostCursorReqDto) {
        Long cursor = getPostCursorReqDto.getCursor();
        Integer size = getPostCursorReqDto.getSize();

        // 첫 페이지 x, db 조회
        if (!cursor.equals(PaginationUtil.START_CURSOR)) {
            List<Post> findPosts = findPostsByCursorCheckExistsCursor(cursor, size);
            List<PostWithCount> findPostsWithCount = findPostsWithCount(findPosts);
            return GetPostCursorResDto.of(findPostsWithCount, size);
        }

        // 커스텀 캐시 존재 x, 캐시 저장
        if (Boolean.FALSE.equals(redisCacheTemplate.hasKey(CacheNames.POST_CURSOR))){
            List<Post> findPosts = findPostsByCursorCheckExistsCursor(cursor, size);
            List<PostWithCount> findPostsWithCount = findPostsWithCount(findPosts);

            ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
            for (Post findPost : findPosts) {
                opsForList.rightPush(CacheNames.POST_CURSOR, findPost);
            }
            opsForList.getOperations().expire(CacheNames.POST_CURSOR, CacheExpireTime.POST_CURSOR, TimeUnit.HOURS);

            return GetPostCursorResDto.of(findPostsWithCount, size);
        }

        // 커스텀 캐시 존재 o, 캐시 조회
        List<Post> cachedPosts = redisCacheTemplate.opsForList().range(CacheNames.POST_CURSOR, 0, -1).stream()
                .map(c -> (Post) c)
                .collect(Collectors.toList());
        List<PostWithCount> findPostsWithCount = findPostsWithCount(cachedPosts);
        return GetPostCursorResDto.of(findPostsWithCount, size);
    }

    public GetPostDetailResDto readPost(Long memberId, Long postId) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));
        Number likeCount = postLikeRepository.countPostLikesByPostId(findPost.getId());
        Number commentCount = commentRepository.countCommentsByPostId(findPost.getId());

        if (memberId != null){
            Member findMember = memberRepository.findMemberById(memberId)
                    .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

            Boolean isLiked = postLikeRepository.findPostLikeByMemberIdAndPostId(findMember.getId(), findPost.getId())
                    .isPresent();

            return GetPostDetailResDto.of(findPost, likeCount, commentCount, isLiked);
        }

        return GetPostDetailResDto.of(findPost, likeCount, commentCount, Boolean.FALSE);
    }

    public PostIdElement createPost(Long memberId, PostPostCreateReqDto postPostCreateReqDto) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        Post post = new Post(
                findMember.getId(),
                postPostCreateReqDto.getTitle(),
                findMember.getName(),
                postPostCreateReqDto.getMusicPath(),
                postPostCreateReqDto.getAlbumImgPath(),
                postPostCreateReqDto.getAlbumName()
        );
        post = postRepository.save(post);

        // 커스텀 캐시 존재 o, 캐시 수정
        if (Boolean.TRUE.equals(redisCacheTemplate.hasKey(CacheNames.POST_CURSOR))) {
            ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
            opsForList.leftPush(CacheNames.POST_CURSOR, post);
            opsForList.rightPop(CacheNames.POST_CURSOR);
        }

        return PostIdElement.of(post);
    }

    public PostIdElement deletePost(Long memberId, Long postId) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        // 음원 게시물 작성 회원이 아닌, 회원이 삭제
        if (!findMember.getId().equals(findPost.getMemberId())){
            throw new BadRequestException(ApplicationError.INVALID_AUTHORITY);
        }

        CompletableFuture.runAsync(() -> postRepository.delete(findPost), threadPoolTaskExecutor);
        CompletableFuture.runAsync(() -> imageService.deleteFile(findPost.getAlbumImgPath()), threadPoolTaskExecutor);
        CompletableFuture.runAsync(() -> imageService.deleteFile(findPost.getMusicPath()), threadPoolTaskExecutor);

        return PostIdElement.of(findPost);
    }

    public PostPostLikeResDto likePost(Long memberId, Long postId) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        PostLike postLike = postLikeRepository.findPostLikeByMemberIdAndPostId(findMember.getId(), findPost.getId())
                .orElse(null);

        Number likeCount = postLikeRepository.countPostLikesByPostId(findPost.getId());

        return togglePostLike(findMember, findPost, postLike, likeCount);
    }

    private PostPostLikeResDto togglePostLike(Member member, Post post, PostLike postLike, Number likeCount) {
        if (postLike != null){
            deleteLike(postLike);
            return PostPostLikeResDto.of(likeCount.intValue() - 1, false);
        }

        saveLike(member, post);
        return PostPostLikeResDto.of(likeCount.intValue() + 1, true);
    }

    private void saveLike(Member member, Post post) {
        PostLike postLike = new PostLike(member.getId(), post.getId());
        postLikeRepository.save(postLike);

        // 캐시 존재 판단에 따른, 캐시 초기화
        if (Boolean.TRUE.equals(redisCacheTemplate.hasKey(getLikeCountKeyByPost(post.getId())))){
            ValueOperations<String, Object> opsForValue = redisCacheTemplate.opsForValue();
            opsForValue.increment(getLikeCountKeyByPost(post.getId()));
        }
    }

    private void deleteLike(PostLike postLike) {
        postLikeRepository.delete(postLike);

        // 캐시 존재 판단에 따른, 캐시 초기화
        if (Boolean.TRUE.equals(redisCacheTemplate.hasKey(getLikeCountKeyByPost(postLike.getPostId())))){
            ValueOperations<String, Object> opsForValue = redisCacheTemplate.opsForValue();
            opsForValue.decrement(getLikeCountKeyByPost(postLike.getPostId()));
        }
    }

    private List<Post> findPostsByCursorCheckExistsCursor(Long cursor, Integer size) {
        return cursor.equals(PaginationUtil.START_CURSOR) ? postRepository.findPostsOrderByIdDesc(size)
                : postRepository.findPostsByIdLessThanOrderByIdDesc(cursor, size);
    }

    private List<PostWithCount> findPostsWithCount(List<Post> findPosts) {
        return findPosts.stream()
                .map(findPost -> {
                    Number findPostLikeCount = postLikeRepository.countPostLikesByPostId(findPost.getId());
                    Number findCommentCount = commentRepository.countCommentsByPostId(findPost.getId());
                    return new PostWithCount(
                            findPost.getId(),
                            findPost.getMemberId(),
                            findPost.getTitle(),
                            findPost.getArtistName(),
                            findPost.getAlbumImgPath(),
                            findPost.getAlbumName(),
                            findPostLikeCount,
                            findCommentCount,
                            findPost.getCreatedAt()
                    );
                })
                .collect(Collectors.toList());
    }

    private String getLikeCountKeyByPost(Long postId) {
        return CacheNames.LIKE_COUNT + "::"
                + "postId_" + postId;
    }
}
