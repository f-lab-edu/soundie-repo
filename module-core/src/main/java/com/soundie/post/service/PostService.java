package com.soundie.post.service;

import com.soundie.comment.repository.CommentRepository;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.global.common.util.CacheNames;
import com.soundie.global.common.util.PaginationUtil;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.domain.PostLike;
import com.soundie.post.domain.PostWithCount;
import com.soundie.post.dto.*;
import com.soundie.post.repository.PostLikeRepository;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PostService {

    private final PostRepository postRepository;
    private final PostLikeRepository postLikeRepository;
    private final MemberRepository memberRepository;
    private final CommentRepository commentRepository;

    private final RedisTemplate<String, Object> redisCacheTemplate;

    public GetPostResDto readPostList(){
        List<Post> findPosts = postRepository.findPosts();
        List<PostWithCount> findPostsWithCount = findPostsWithCount(findPosts);

        return GetPostResDto.of(findPostsWithCount);
    }

    public GetPostCursorResDto readPostListByCursor(GetPostCursorReqDto getPostCursorReqDto) {
        Long cursor = getPostCursorReqDto.getCursor();
        Integer size = getPostCursorReqDto.getSize();

        // 캐시 존재 판단에 따른, 캐시 리턴
        if (redisCacheTemplate.hasKey(getPostKeyByCursorAndSize(cursor, size))) {
            return GetPostCursorResDto.of(
                    redisCacheTemplate.opsForList().range(getPostKeyByCursorAndSize(cursor, size), 0, -1).stream()
                            .map(v -> (PostWithCount) v)
                            .collect(Collectors.toList())
                    , size
            );
        }

        // 캐시 존재 판단에 따른, 캐시 저장 및 DB 리턴
        List<Post> findPosts = findPostsByCursorCheckExistsCursor(cursor, size);
        List<PostWithCount> findPostsWithCount = findPostsWithCount(findPosts);

        ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
        for (PostWithCount postWithCount : findPostsWithCount) {
            opsForList.rightPush(getPostKeyByCursorAndSize(cursor, size), postWithCount);
        }
        opsForList.getOperations().expire(getPostKeyByCursorAndSize(cursor, size), 1L, TimeUnit.HOURS);

        return GetPostCursorResDto.of(findPostsWithCount, size);
    }

    @Cacheable(cacheNames = CacheNames.POST, key = "'postId_' + #postId + ':memberId_' + #memberId")
    public GetPostDetailResDto readPost(Long memberId, Long postId) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));
        Long likeCount = postLikeRepository.countPostLikesByPostId(findPost.getId());
        Long commentCount = commentRepository.countCommentsByPostId(findPost.getId());

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

        // 캐시 존재 판단에 따른, 캐시 초기화
        if (redisCacheTemplate.hasKey(getPostKeyByCursorAndSize(PaginationUtil.START_CURSOR, PaginationUtil.POST_SIZE))) {
            ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
            opsForList.leftPush(
                    getPostKeyByCursorAndSize(PaginationUtil.START_CURSOR, PaginationUtil.POST_SIZE),
                    new PostWithCount(
                            post.getId(),
                            post.getMemberId(),
                            post.getTitle(),
                            post.getArtistName(),
                            post.getAlbumImgPath(),
                            post.getAlbumName(),
                            0,
                            0,
                            post.getCreatedAt()
                    ));
            opsForList.rightPop(
                    getPostKeyByCursorAndSize(PaginationUtil.START_CURSOR, PaginationUtil.POST_SIZE)
            );
        }

        return PostIdElement.of(post);
    }

    public PostPostLikeResDto likePost(Long memberId, Long postId) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        PostLike postLike = postLikeRepository.findPostLikeByMemberIdAndPostId(findMember.getId(), findPost.getId())
                .orElse(null);

        Long likeCount = postLikeRepository.countPostLikesByPostId(findPost.getId());

        return togglePostLike(findMember, findPost, postLike, likeCount);
    }

    private PostPostLikeResDto togglePostLike(Member member, Post post, PostLike postLike, Long likeCount) {
        if (postLike != null){
            deleteLike(postLike);
            return PostPostLikeResDto.of(likeCount - 1, false);
        }

        saveLike(member, post);
        return PostPostLikeResDto.of(likeCount + 1, true);
    }

    private void saveLike(Member member, Post post) {
        PostLike postLike = new PostLike(member.getId(), post.getId());
        postLikeRepository.save(postLike);
    }

    private void deleteLike(PostLike postLike) {
        postLikeRepository.delete(postLike);
    }

    private List<Post> findPostsByCursorCheckExistsCursor(Long cursor, Integer size) {
        return cursor.equals(PaginationUtil.START_CURSOR) ? postRepository.findPostsByOrderByIdDescCreatedAtDesc(size)
                : postRepository.findPostsByIdLessThanOrderByIdDescCreatedAtDesc(cursor, size);
    }

    private List<PostWithCount> findPostsWithCount(List<Post> findPosts) {
        return findPosts.stream()
                .map(findPost -> {
                    Long findPostLikeCount = postLikeRepository.countPostLikesByPostId(findPost.getId());
                    Long findCommentCount = commentRepository.countCommentsByPostId(findPost.getId());
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

    private String getPostKeyByCursorAndSize(Long cursor, Integer size) {
        return CacheNames.POST + "::"
                + "cursor_" + cursor
                + ":size_" + size;
    }
}
