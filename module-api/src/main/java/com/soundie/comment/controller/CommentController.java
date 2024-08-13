package com.soundie.comment.controller;

import com.soundie.comment.dto.CommentIdElement;
import com.soundie.comment.dto.GetCommentResDto;
import com.soundie.comment.dto.PostCommentCreateReqDto;
import com.soundie.comment.service.CommentService;
import com.soundie.global.common.EnvelopeResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/comments")
public class CommentController {

    private final CommentService commentService;

    @GetMapping
    public EnvelopeResponse readComments(@RequestParam Long postId,
                                         @RequestParam Long memberId){
        GetCommentResDto commentList = commentService.readCommentList(memberId, postId);
        return new EnvelopeResponse<>("200", "success", commentList);
    }

    @PostMapping
    public EnvelopeResponse createComment(@RequestBody PostCommentCreateReqDto postCommentCreateReqDto,
                                          @RequestParam Long postId,
                                          @RequestParam Long memberId){
        CommentIdElement commentId = commentService.createComment(memberId, postId, postCommentCreateReqDto);
        return new EnvelopeResponse<>("200", "success", commentId);
    }
}
