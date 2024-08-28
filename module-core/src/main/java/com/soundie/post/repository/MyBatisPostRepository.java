package com.soundie.post.repository;

import com.soundie.post.domain.Post;
import com.soundie.post.mapper.PostMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MyBatisPostRepository implements PostRepository {

    private final PostMapper postMapper;

    /*
     * 음원 게시물 목록 조회
     * */
    @Override
    public List<Post> findPosts() {
        return postMapper.findPosts();
    }

    /*
     * 음원 게시물 Id로, 음원 게시물 조회
     * */
    @Override
    public Optional<Post> findPostById(Long postId) {
        return postMapper.findPostById(postId)
                .map(postVo -> new Post(
                        postVo.getId(),
                        postVo.getMemberId(),
                        postVo.getTitle(),
                        postVo.getArtistName(),
                        postVo.getMusicPath(),
                        postVo.getAlbumImgPath(),
                        postVo.getAlbumName(),
                        postVo.getCreatedAt()
                ));
    }

    /*
     * 음원 게시물 저장
     * */
    @Override
    public Post save(Post post) {
        postMapper.save(post);
        return post;
    }
}
