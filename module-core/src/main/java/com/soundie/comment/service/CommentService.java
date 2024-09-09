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
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class CommentService {

    private final CommentRepository commentRepository;
    private final MemberRepository memberRepository;
    private final PostRepository postRepository;

    public GetCommentResDto readCommentList(Long postId) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        List<Comment> comments = commentRepository.findCommentsByPostId(findPost.getId());

        Map<Long, Member> linkedHashMap = new LinkedHashMap<>();
        for (Comment comment : comments){
            Member findMember = memberRepository.findMemberById(comment.getMemberId())
                    .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
            linkedHashMap.put(comment.getId(), findMember);
        }

        return GetCommentResDto.of(comments, linkedHashMap);
    }

    public GetCommentCursorResDto readCommentListByCursor(Long postId, GetCommentCursorReqDto getCommentCursorReqDto) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        Long cursor = getCommentCursorReqDto.getCursor();
        Integer size = getCommentCursorReqDto.getSize();
        List<Comment> comments = findCommentsByCursorCheckExistsCursor(findPost.getId(), cursor, size);

        Map<Long, Member> linkedHashMap = new LinkedHashMap<>();
        for (Comment comment : comments){
            Member findMember = memberRepository.findMemberById(comment.getMemberId())
                    .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
            linkedHashMap.put(comment.getId(), findMember);
        }

        return GetCommentCursorResDto.of(comments, linkedHashMap, size);
    }

    private List<Comment> findCommentsByCursorCheckExistsCursor(Long postId, Long cursor, Integer size) {
        return cursor == null ? commentRepository.findCommentsByPostIdOrderByIdAscCreatedAtAsc(postId, size)
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
}
