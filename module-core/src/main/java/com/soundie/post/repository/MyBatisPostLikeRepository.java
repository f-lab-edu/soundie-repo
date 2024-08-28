package com.soundie.post.repository;

import com.soundie.post.domain.PostLike;
import com.soundie.post.mapper.PostLikeMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MyBatisPostLikeRepository implements PostLikeRepository{

    private final PostLikeMapper postLikeMapper;

    /*
     * 좋아요 목록 조회
     * */
    @Override
    public List<PostLike> findPostLikes() {
        return postLikeMapper.findPostLikes();
    }

    /*
     * 회원 Id + 음원 게시물 Id로, 좋아요 조회
     * */
    @Override
    public Optional<PostLike> findPostLikeByMemberIdAndPostId(Long memberId, Long postId) {
        return postLikeMapper.findPostLikeByMemberIdAndPostId(memberId, postId);
    }

    /*
     * 좋아요 개수 조회
     * */
    @Override
    public Long countPostLikesByPostId(Long postId) {
        return postLikeMapper.countPostLikesByPostId(postId);
    }

    /*
     * 좋아요 저장
     * */
    @Override
    public PostLike save(PostLike postLike) {
        postLikeMapper.save(postLike);
        return postLike;
    }

    /*
     * 좋아요 삭제
     * */
    @Override
    public void delete(PostLike postLike) {
        postLikeMapper.delete(postLike);
    }
}