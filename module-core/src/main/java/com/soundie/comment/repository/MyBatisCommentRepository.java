package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.mapper.CommentMapper;
import com.soundie.global.common.util.CacheNames;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class MyBatisCommentRepository implements CommentRepository {

    private final CommentMapper commentMapper;

    /*
     * 댓글 목록 조회
     * */
    @Override
    public List<Comment> findComments() {
        return commentMapper.findComments();
    }

    /*
     * 음원 게시물 Id로, 댓글 목록 조회
     * */
    @Override
    public List<Comment> findCommentsByPostId(Long postId) {
        return commentMapper.findCommentsByPostId(postId);
    }

    @Override
    public List<Comment> findCommentsByPostIdOrderByIdAscCreatedAtAsc(Long postId, Integer size) {
        return commentMapper.findCommentsByPostIdOrderByIdAscCreatedAtAsc(postId, size);
    }

    @Override
    public List<Comment> findCommentsByPostIdAndIdLessThanOrderByIdAscCreatedAtAsc(Long postId, Long cursor, Integer size) {
        return commentMapper.findCommentsByPostIdAndIdLessThanOrderByIdAscCreatedAtAsc(postId, cursor, size);
    }

    /*
     * 음원 게시물 Id로, 댓글 개수 조회
     * */
    @Override
    @Cacheable(cacheNames = CacheNames.COMMENT_COUNT, key = "'postId_' + #postId")
    public Number countCommentsByPostId(Long postId){
        return commentMapper.countCommentsByPostId(postId);
    }

    /*
     * 댓글 저장
     * */
    @Override
    public Comment save(Comment comment) {
        commentMapper.save(comment);
        return comment;
    }
}
