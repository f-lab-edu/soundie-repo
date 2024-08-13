package com.soundie.comment.service;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.dto.CommentIdElement;
import com.soundie.comment.dto.GetCommentResDto;
import com.soundie.comment.dto.PostCommentCreateReqDto;
import com.soundie.comment.repository.CommentRepository;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class CommentService {

    private final CommentRepository commentRepository;
    private final MemberRepository memberRepository;
    private final PostRepository postRepository;

    public GetCommentResDto readCommentList(Long memberId, Long postId) {
        // 수정 필요: postId 존재 판단
        Member member = memberRepository.findMemberById(memberId);
        List<Comment> findComments = commentRepository.findCommentsByMemberIdAndPostId(memberId, postId);
        return GetCommentResDto.of(findComments, member);
    }

    public CommentIdElement createComment(Long memberId, Long postId, PostCommentCreateReqDto postCommentCreateReqDto) {
        // 수정 필요: postId 존재 판단
        Post post = postRepository.findPostById(postId);

        Comment comment = new Comment(
                memberId,
                postId,
                postCommentCreateReqDto.getContent()
        );

        post.getComments().add(comment);

        comment = commentRepository.save(comment);

        return new CommentIdElement(comment.getId());
    }
}
