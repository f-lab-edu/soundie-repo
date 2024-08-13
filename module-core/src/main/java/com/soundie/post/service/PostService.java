package com.soundie.post.service;

import com.soundie.member.domain.Member;
import com.soundie.member.repository.MemberRepository;
import com.soundie.post.domain.Post;
import com.soundie.post.domain.PostLike;
import com.soundie.post.dto.*;
import com.soundie.post.repository.PostLikeRepository;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PostService {

    private final PostRepository postRepository;
    private final PostLikeRepository postLikeRepository;
    private final MemberRepository memberRepository;

    public List<GetPostResDto> readPostList(){
        List<Post> findPosts = postRepository.findPosts();
        return findPosts.stream()
                .map(p -> new GetPostResDto(
                        p.getId(),
                        p.getTitle(),
                        p.getArtistName(),
                        p.getMusicPath(),
                        p.getAlbumImgPath(),
                        p.getAlbumName(),
                        p.getLikes().size(),
                        p.getComments().size(),
                        p.getCreatedAt(),
                        p.getLikes().size() != 0 // 수정 필요
                ))
                .collect(Collectors.toList());
    }

    public GetPostDetailResDto readPost(Long postId) {
        Post findPost = postRepository.findPostById(postId);
        return new GetPostDetailResDto(
                findPost.getId(),
                findPost.getTitle(),
                findPost.getArtistName(),
                findPost.getMusicPath(),
                findPost.getAlbumImgPath(),
                findPost.getAlbumName(),
                findPost.getLikes().size(),
                findPost.getComments().size(),
                findPost.getCreatedAt(),
                findPost.getLikes().size() != 0 // 수정 필요
        );
    }

    public PostIdElement createPost(Long memberId, PostPostCreateReqDto postPostCreateReqDto) {
        // 수정 필요: 회원 Id로 Post 등록
        Post post = new Post(
                postPostCreateReqDto.getTitle(),
                postPostCreateReqDto.getArtistName(),
                postPostCreateReqDto.getMusicPath(),
                postPostCreateReqDto.getAlbumImgPath(),
                postPostCreateReqDto.getAlbumName(),
                "2024-08-13");
        Long postId = postRepository.save(post).getId();
        return new PostIdElement(postId);
    }

    public PostCommonLikeResDto likePost(Long memberId, Long postId) {
        Member member = memberRepository.findMemberById(memberId);
        Post post = postRepository.findPostById(postId);
        PostLike postLike = postLikeRepository.findByMemberIdAndPostId(memberId, postId)
                .orElse(null);

        Long likeCount = postLikeRepository.countByPostId(post.getId());

        return togglePostLike(member, post, postLike, likeCount);
    }

    private PostCommonLikeResDto togglePostLike(Member member, Post post, PostLike postLike, Long likeCount) {
        if (postLike != null){
            deleteLike(post, postLike);
            return new PostCommonLikeResDto(likeCount - 1, false);
        }

        saveLike(member, post);
        return new PostCommonLikeResDto(likeCount + 1, true);
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
