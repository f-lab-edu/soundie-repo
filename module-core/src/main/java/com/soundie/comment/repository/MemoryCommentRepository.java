package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

public class MemoryCommentRepository implements CommentRepository {

    private final Map<Long, Comment> store = new ConcurrentHashMap<>();
    private AtomicLong sequence = new AtomicLong(0L);

    /*
     * 댓글 목록 조회
    * */
    @Override
    public List<Comment> findComments() {
        return new ArrayList<>(store.values());
    }


    /*
     * 음원 게시물 Id로, 댓글 목록 조회
     * */
    @Override
    public List<Comment> findCommentsByPostId(Long postId) {
        return findComments().stream()
                .filter(c -> c.getPostId().equals(postId))
                .collect(Collectors.toList());
    }

    /*
    * 음원 게시물 Id로, 댓글 개수 조회
    * */
    @Override
    public Long countCommentsByPostId(Long postId){
        return findComments().stream()
                .filter(c -> c.getPostId().equals(postId))
                .count();
    }

    /*
     * 댓글 저장
     * */
    @Override
    public Comment save(Comment comment){
        comment.setId(sequence.incrementAndGet());
        store.put(comment.getId(), comment);

        return comment;
    }

    public void clearStore() {
        store.clear();
    }
}
