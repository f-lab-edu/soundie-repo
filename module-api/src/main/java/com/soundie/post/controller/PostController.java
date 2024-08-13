package com.soundie.post.controller;

import com.soundie.global.common.EnvelopeResponse;
import com.soundie.post.dto.GetPostResDto;
import com.soundie.post.service.PostService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
}
