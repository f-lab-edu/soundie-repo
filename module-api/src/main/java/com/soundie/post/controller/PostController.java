package com.soundie.post.controller;

import com.soundie.global.common.EnvelopeResponse;
import com.soundie.post.dto.*;
import com.soundie.post.service.PostService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/posts")
public class PostController {

    private final PostService postService;

    @GetMapping
    public EnvelopeResponse readPostList(){
        List<GetPostResDto> postList = postService.readPostList();
        return new EnvelopeResponse<>("200", "success", postList);
    }

    @GetMapping("/{postId}")
    public EnvelopeResponse<GetPostDetailResDto> readPost(@PathVariable Long postId){
        return EnvelopeResponse.<GetPostDetailResDto>builder()
                .data(postService.readPost(postId))
                .build();
    }

    @PostMapping
    public EnvelopeResponse<PostIdElement> createPost(@RequestBody PostPostCreateReqDto postPostCreateReqDto,
                                       @RequestParam Long memberId){
        return EnvelopeResponse.<PostIdElement>builder()
                .data(postService.createPost(memberId, postPostCreateReqDto))
                .build();
    }

    @PostMapping("/{postId}/like")
    public EnvelopeResponse<PostCommonLikeResDto> likePost(@PathVariable Long postId,
                                     @RequestParam Long memberId){
        return EnvelopeResponse.<PostCommonLikeResDto>builder()
                .data(postService.likePost(memberId, postId))
                .build();
    }
}
