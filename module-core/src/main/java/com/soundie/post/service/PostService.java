package com.soundie.post.service;

import com.soundie.post.domain.Post;
import com.soundie.post.dto.GetPostResDto;
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
}
