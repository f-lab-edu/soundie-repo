package com.soundie.api;

import com.soundie.comment.dto.*;
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

    @GetMapping("/cursor")
    public EnvelopeResponse<GetCommentCursorResDto> readCommentsByCursor(@RequestBody GetCommentCursorReqDto getCommentCursorReqDto,
                                                                         @RequestParam Long postId,
                                                                         @RequestParam(required = false) Long memberId){
        return EnvelopeResponse.<GetCommentCursorResDto>builder()
                .data(commentService.readCommentListByCursor(postId, getCommentCursorReqDto))
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
