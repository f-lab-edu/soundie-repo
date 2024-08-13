package com.soundie.post.service;

import com.soundie.post.domain.Post;
import com.soundie.post.dto.GetPostDetailResDto;
import com.soundie.post.dto.GetPostResDto;
import com.soundie.post.dto.PostIdElement;
import com.soundie.post.dto.PostPostCreateReqDto;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PostService {

    private final PostRepository postRepository;

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
                        false
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
                false
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
}
