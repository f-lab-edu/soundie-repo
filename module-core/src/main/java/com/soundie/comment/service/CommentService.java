package com.soundie.comment.service;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.domain.CommentWithAuthor;
import com.soundie.comment.dto.CommentIdElement;
import com.soundie.comment.dto.GetCommentResDto;
import com.soundie.comment.dto.PostCommentCreateReqDto;
import com.soundie.comment.dto.GetCommentCursorReqDto;
import com.soundie.comment.dto.GetCommentCursorResDto;
import com.soundie.comment.repository.CommentRepository;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.global.common.util.CacheExpireTime;
import com.soundie.global.common.util.CacheNames;
import com.soundie.global.common.util.PaginationUtil;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.ListOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CommentService {

    private final CommentRepository commentRepository;
    private final MemberRepository memberRepository;
    private final PostRepository postRepository;

    private final RedisTemplate<String, Object> redisCacheTemplate;

    public GetCommentResDto readCommentList(Long postId) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        List<CommentWithAuthor> commentsWithAuthor = commentRepository.findCommentsWithAuthorByPostId(findPost.getId());
        return GetCommentResDto.of(commentsWithAuthor);
    }

    public GetCommentCursorResDto readCommentListByCursor(Long postId, GetCommentCursorReqDto getCommentCursorReqDto) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        Long cursor = getCommentCursorReqDto.getCursor();
        Integer size = getCommentCursorReqDto.getSize();
        
        // 캐시 존재 판단에 따른, 캐시 저장
        if (Boolean.FALSE.equals(redisCacheTemplate.hasKey(getCommentKeyByPostAndCursor(postId, cursor)))){
            List<CommentWithAuthor> commentsWithAuthor = findCommentsWithAuthorByCursorCheckExistsCursor(findPost.getId(), cursor, size);
            ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
            for (CommentWithAuthor findCommentWithAuthor : commentsWithAuthor) {
                opsForList.rightPush(getCommentKeyByPostAndCursor(postId, cursor), findCommentWithAuthor);
            }
            opsForList.getOperations().expire(getCommentKeyByPostAndCursor(postId, cursor), CacheExpireTime.COMMENT, TimeUnit.HOURS);

            return GetCommentCursorResDto.of(commentsWithAuthor, size);
        } 

        // 캐시 조회
        List<CommentWithAuthor> cachedCommentsWithAuthor = redisCacheTemplate.opsForList().range(getCommentKeyByPostAndCursor(postId, cursor), 0, -1).stream()
                .map(v -> (CommentWithAuthor) v)
                .collect(Collectors.toList());

        // 마지막 페이지 판단에 따른, 캐시 저장
        if (cachedCommentsWithAuthor.size() < size) {
            List<CommentWithAuthor> commentsWithAuthor = findCommentsWithAuthorByCursorCheckExistsCursor(findPost.getId(), cursor, size);
            List<CommentWithAuthor> filteredComments = commentsWithAuthor.stream()
                    .filter(commentWithAuthor -> cachedCommentsWithAuthor.stream()
                            .noneMatch(cachedCommentWithAuthor -> commentWithAuthor.getId().equals(cachedCommentWithAuthor.getId()))
                    )
                    .collect(Collectors.toList());
            ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
            for (CommentWithAuthor filteredComment : filteredComments) {
                opsForList.rightPush(getCommentKeyByPostAndCursor(postId, cursor), filteredComment);
            }

            return GetCommentCursorResDto.of(commentsWithAuthor, size);
        }

        return GetCommentCursorResDto.of(cachedCommentsWithAuthor, size);
    }

    public CommentIdElement createComment(Long memberId, Long postId, PostCommentCreateReqDto postCommentCreateReqDto) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        Comment comment = new Comment(
                findMember.getId(),
                findPost.getId(),
                postCommentCreateReqDto.getContent()
        );

        comment = commentRepository.save(comment);

        // 캐시 존재 판단에 따른, 캐시 초기화
        if (Boolean.TRUE.equals(redisCacheTemplate.hasKey(getCommentCountKeyByPost(comment.getPostId())))){
            ValueOperations<String, Object> opsForValue = redisCacheTemplate.opsForValue();
            opsForValue.increment(getCommentCountKeyByPost(comment.getPostId()));
        }

        return CommentIdElement.of(comment);
    }

    private List<CommentWithAuthor> findCommentsWithAuthorByCursorCheckExistsCursor(Long postId, Long cursor, Integer size) {
        return cursor.equals(PaginationUtil.START_CURSOR) ? commentRepository.findCommentsWithAuthorByPostIdOrderByIdAsc(postId, size)
                : commentRepository.findCommentsWithAuthorByPostIdAndIdLessThanOrderByIdAsc(postId, cursor, size);
    }

    private String getCommentKeyByPostAndCursor(Long postId, Long cursor) {
        return CacheNames.COMMENT + "::"
                + "postId_" + postId
                + ":cursor_" + cursor;
    }

    private String getCommentCountKeyByPost(Long postId) {
        return CacheNames.COMMENT_COUNT + "::"
                + "postId_" + postId;
    }
}
