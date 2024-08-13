package com.soundie.post.repository;

import com.soundie.post.domain.PostLike;
import org.springframework.stereotype.Repository;

import java.util.*;

@Repository
public class PostLikeRepository {

    private static final Map<Long, PostLike> store = new HashMap<>(); //static
    private static long sequence = 0L; //static

    /*
     * 좋아요 목록 조회
     * */
    public List<PostLike> findPostLikes() {
        return new ArrayList<>(store.values());
    }

    /*
     * 회원 Id + 음원 게시물 Id로, 좋아요 조회
     * */
    public Optional<PostLike> findPostLikeByMemberIdAndPostId(Long memberId, Long postId) {
        return findPostLikes().stream()
                .filter(pl -> pl.getMemberId().equals(memberId) && pl.getPostId().equals(postId) )
                .findFirst();
    }

    /*
     * 좋아요 개수 조회
     * */
    public Long countPostLikesByPostId(Long postId){
        return findPostLikes().stream()
                .filter(pl -> pl.getPostId().equals(postId))
                .count();
    }

    /*
     * 좋아요 저장
     * */
    public PostLike save(PostLike postLike) {
        postLike.setId(++sequence);
        store.put(postLike.getId(), postLike);

        return postLike;
    }

    /*
    * 좋아요 삭제
    * */
    public void delete(PostLike postLike){
        store.remove(postLike.getId());
    }
}
