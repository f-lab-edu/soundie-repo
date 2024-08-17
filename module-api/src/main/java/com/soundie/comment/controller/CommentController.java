package com.soundie.comment.controller;

import com.soundie.comment.dto.CommentIdElement;
import com.soundie.comment.dto.GetCommentResDto;
import com.soundie.comment.dto.PostCommentCreateReqDto;
import com.soundie.comment.service.CommentService;
import com.soundie.global.common.dto.EnvelopeResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/comments")
public class CommentController {

    private final CommentService commentService;

    @GetMapping
    public EnvelopeResponse<GetCommentResDto> readComments(@RequestParam Long postId,
                                                           @RequestParam(required = false) Long memberId){
        return EnvelopeResponse.<GetCommentResDto>builder()
                .data(commentService.readCommentList(postId))
                .build();
    }

    @PostMapping
    public EnvelopeResponse<CommentIdElement> createComment(@RequestBody PostCommentCreateReqDto postCommentCreateReqDto,
                                                            @RequestParam Long postId,
                                                            @RequestParam Long memberId){
        return EnvelopeResponse.<CommentIdElement>builder()
                .data(commentService.createComment(memberId, postId, postCommentCreateReqDto))
                .build();
    }
}
