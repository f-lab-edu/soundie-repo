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

        findPost.getComments().add(comment);

        comment = commentRepository.save(comment);

        return CommentIdElement.of(comment.getId());
    }
}
