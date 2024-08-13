package com.soundie.post.controller;

import com.soundie.global.common.EnvelopeResponse;
import com.soundie.post.dto.GetPostDetailResDto;
import com.soundie.post.dto.GetPostResDto;
import com.soundie.post.dto.PostIdElement;
import com.soundie.post.dto.PostPostCreateReqDto;
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
    public EnvelopeResponse readPost(@PathVariable Long postId){
        GetPostDetailResDto post = postService.readPost(postId);
        return new EnvelopeResponse<>("200", "success", post);
    }

    @PostMapping
    public EnvelopeResponse createPost(@RequestBody PostPostCreateReqDto postPostCreateReqDto,
                                       @RequestParam Long memberId){
        PostIdElement postId = postService.createPost(memberId, postPostCreateReqDto);
        return new EnvelopeResponse("200", "success", postId);
    }
}
