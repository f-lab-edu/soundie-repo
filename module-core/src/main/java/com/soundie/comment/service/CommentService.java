package com.soundie.comment.service;

import com.soundie.comment.domain.Comment;
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
import org.springframework.stereotype.Service;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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

        List<Comment> comments = commentRepository.findCommentsByPostId(findPost.getId());
        Map<Long, Member> findCommentsByMember = findCommentsByMember(comments);

        return GetCommentResDto.of(comments, findCommentsByMember);
    }

    public GetCommentCursorResDto readCommentListByCursor(Long postId, GetCommentCursorReqDto getCommentCursorReqDto) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        Long cursor = getCommentCursorReqDto.getCursor();
        Integer size = getCommentCursorReqDto.getSize();
        
        // 캐시 존재 판단에 따른, 캐시 저장
        if (Boolean.FALSE.equals(redisCacheTemplate.hasKey(getCommentKeyByPostAndCursorAndSize(postId, cursor, size)))){
            List<Comment> comments = findCommentsByCursorCheckExistsCursor(findPost.getId(), cursor, size);
            ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
            for (Comment findComment : comments) {
                opsForList.rightPush(getCommentKeyByPostAndCursorAndSize(postId, cursor, size), findComment);
            }
            opsForList.getOperations().expire(getCommentKeyByPostAndCursorAndSize(postId, cursor, size), CacheExpireTime.COMMENT, TimeUnit.HOURS);

            Map<Long, Member> findCommentsByMember = findCommentsByMember(comments);
            return GetCommentCursorResDto.of(comments, findCommentsByMember, size);
        } 

        // 캐시 조회
        List<Comment> cachedComments = redisCacheTemplate.opsForList().range(getCommentKeyByPostAndCursorAndSize(postId, cursor, size), 0, -1).stream()
                .map(v -> (Comment) v)
                .collect(Collectors.toList());

        // 마지막 페이지 판단에 따른, 캐시 저장
        if (cachedComments.size() < size) {
            List<Comment> comments = findCommentsByCursorCheckExistsCursor(findPost.getId(), cursor, size);
            List<Comment> filteredComments = comments.stream()
                    .filter(comment -> cachedComments.stream()
                            .noneMatch(cachedComment -> comment.getId().equals(cachedComment.getId())))
                    .collect(Collectors.toList());
            ListOperations<String, Object> opsForList = redisCacheTemplate.opsForList();
            for (Comment filteredComment : filteredComments) {
                opsForList.rightPush(getCommentKeyByPostAndCursorAndSize(postId, cursor, size), filteredComment);
            }

            Map<Long, Member> findCommentsByMember = findCommentsByMember(comments);
            return GetCommentCursorResDto.of(comments, findCommentsByMember, size);
        }
        
        Map<Long, Member> findCommentsByMember = findCommentsByMember(cachedComments);
        return GetCommentCursorResDto.of(cachedComments, findCommentsByMember, size);
    }

    private List<Comment> findCommentsByCursorCheckExistsCursor(Long postId, Long cursor, Integer size) {
        return cursor.equals(PaginationUtil.START_CURSOR) ? commentRepository.findCommentsByPostIdOrderByIdAscCreatedAtAsc(postId, size)
                : commentRepository.findCommentsByPostIdAndIdLessThanOrderByIdAscCreatedAtAsc(postId, cursor, size);
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

        return CommentIdElement.of(comment);
    }

    private Map<Long, Member> findCommentsByMember(List<Comment> comments) {
        Map<Long, Member> findCommentsByMember = new LinkedHashMap<>();
        for (Comment comment : comments){
            Member findMember = memberRepository.findMemberById(comment.getMemberId())
                    .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
            findCommentsByMember.put(comment.getId(), findMember);
        }

        return findCommentsByMember;
    }

    private String getCommentKeyByPostAndCursorAndSize(Long postId, Long cursor, Integer size) {
        return CacheNames.COMMENT + "::"
                + "postId_" + postId
                + ":cursor_" + cursor
                + ":size_" + size;
    }
}
