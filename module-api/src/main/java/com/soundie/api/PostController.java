package com.soundie.api;

import com.soundie.global.common.dto.EnvelopeResponse;
import com.soundie.post.dto.GetPostCursorReqDto;
import com.soundie.post.dto.GetPostCursorResDto;
import com.soundie.post.dto.GetPostDetailResDto;
import com.soundie.post.dto.GetPostResDto;
import com.soundie.post.dto.PostIdElement;
import com.soundie.post.dto.PostPostCreateReqDto;
import com.soundie.post.dto.PostPostLikeResDto;
import com.soundie.post.service.PostService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/posts")
public class PostController {

    private final PostService postService;

    @GetMapping
    public EnvelopeResponse<GetPostResDto> readPosts(@RequestParam(required = false) Long memberId){
        return EnvelopeResponse.<GetPostResDto>builder()
                .data(postService.readPostList())
                .build();
    }

    @GetMapping("/cursor")
    public EnvelopeResponse<GetPostCursorResDto> readPostsByCursor(@RequestBody GetPostCursorReqDto getPostCursorReqDto,
                                                                   @RequestParam(required = false) Long memberId) {
        return EnvelopeResponse.<GetPostCursorResDto>builder()
                .data(postService.readPostListByCursor(getPostCursorReqDto))
                .build();
    }

    @GetMapping("/{postId}")
    public EnvelopeResponse<GetPostDetailResDto> readPost(@PathVariable Long postId,
                                                          @RequestParam(required = false) Long memberId){
        return EnvelopeResponse.<GetPostDetailResDto>builder()
                .data(postService.readPost(memberId, postId))
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
    public EnvelopeResponse<PostPostLikeResDto> likePost(@PathVariable Long postId,
                                                         @RequestParam Long memberId){
        return EnvelopeResponse.<PostPostLikeResDto>builder()
                .data(postService.likePost(memberId, postId))
                .build();
    }

    @DeleteMapping("/{postId}")
    public EnvelopeResponse<PostIdElement> deletePost(@PathVariable Long postId,
                                                      @RequestParam Long memberId) {
        return EnvelopeResponse.<PostIdElement>builder()
                .data(postService.deletePost(memberId, postId))
                .build();
    }
}
