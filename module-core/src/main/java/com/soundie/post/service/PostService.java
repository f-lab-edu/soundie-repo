package com.soundie.post.service;

import com.soundie.global.common.exception.ApplicationError;
import com.soundie.global.common.exception.NotFoundException;
import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemoryMemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.domain.PostLike;
import com.soundie.post.dto.*;
import com.soundie.post.repository.MemoryPostLikeRepository;
import com.soundie.post.repository.MemoryPostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PostService {

    private final MemoryPostRepository postRepository;
    private final MemoryPostLikeRepository postLikeRepository;
    private final MemoryMemberRepository memberRepository;

    public GetPostResDto readPostList(){
        List<Post> findPosts = postRepository.findPosts();
        return GetPostResDto.of(findPosts);
    }

    public GetPostDetailResDto readPost(Long memberId, Long postId) {
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        if (memberId != null){
            Member findMember = memberRepository.findMemberById(memberId)
                    .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
            return GetPostDetailResDto.of(findPost, findMember);
        }

        return GetPostDetailResDto.of(findPost, null);
    }

    public PostIdElement createPost(Long memberId, PostPostCreateReqDto postPostCreateReqDto) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));

        Post post = new Post(
                findMember.getId(),
                postPostCreateReqDto.getTitle(),
                findMember.getName(),
                postPostCreateReqDto.getMusicPath(),
                postPostCreateReqDto.getAlbumImgPath(),
                postPostCreateReqDto.getAlbumName()
        );

        post = postRepository.save(post);

        return PostIdElement.of(post.getId());
    }

    public PostPostLikeResDto likePost(Long memberId, Long postId) {
        Member findMember = memberRepository.findMemberById(memberId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.MEMBER_NOT_FOUND));
        Post findPost = postRepository.findPostById(postId)
                .orElseThrow(() -> new NotFoundException(ApplicationError.POST_NOT_FOUND));

        PostLike postLike = postLikeRepository.findPostLikeByMemberIdAndPostId(findMember.getId(), findPost.getId())
                .orElse(null);

        Long likeCount = postLikeRepository.countPostLikesByPostId(findPost.getId());

        return togglePostLike(findMember, findPost, postLike, likeCount);
    }

    private PostPostLikeResDto togglePostLike(Member member, Post post, PostLike postLike, Long likeCount) {
        if (postLike != null){
            deleteLike(post, postLike);
            return PostPostLikeResDto.of(likeCount - 1, false);
        }

        saveLike(member, post);
        return PostPostLikeResDto.of(likeCount + 1, true);
    }

    private void saveLike(Member member, Post post) {
        PostLike postLike = new PostLike(member.getId(), post.getId());

        post.getLikes().add(postLike);

        postLikeRepository.save(postLike);
    }

    private void deleteLike(Post post, PostLike postLike) {
        post.getLikes().stream()
                        .filter(pl -> pl.getId().equals(postLike.getId()))
                        .collect(Collectors.toList())
                        .forEach(x -> post.getLikes().remove(x));

        postLikeRepository.delete(postLike);
    }
}
