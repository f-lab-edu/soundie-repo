package com.soundie.comment.service;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.dto.CommentIdElement;
import com.soundie.comment.dto.GetCommentResDto;
import com.soundie.comment.dto.PostCommentCreateReqDto;
import com.soundie.comment.repository.MemoryCommentRepository;
import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemoryMemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.repository.MemoryPostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class CommentService {

    private final MemoryCommentRepository commentRepository;
    private final MemoryMemberRepository memberRepository;
    private final MemoryPostRepository postRepository;

    public GetCommentResDto readCommentList(Long postId) {
        // 수정 필요: postId 존재 판단
        List<Comment> findComments = commentRepository.findCommentsByPostId(postId);

        Map<Long, Member> linkedHashMap = new LinkedHashMap<>();
        for (Comment comment : findComments){
            Member member = memberRepository.findMemberById(comment.getMemberId())
                    .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
            linkedHashMap.put(comment.getId(), member);
        }

        return GetCommentResDto.of(findComments, linkedHashMap);
    }

    public CommentIdElement createComment(Long memberId, Long postId, PostCommentCreateReqDto postCommentCreateReqDto) {
        // 수정 필요: postId 존재 판단
        Post post = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        Comment comment = new Comment(
                memberId,
                postId,
                postCommentCreateReqDto.getContent()
        );

        post.getComments().add(comment);

        comment = commentRepository.save(comment);

        return CommentIdElement.of(comment.getId());
    }
}
