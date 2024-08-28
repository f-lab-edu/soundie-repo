package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.mapper.CommentMapper;
import lombok.RequiredArgsConstructor;
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

    /*
     * 댓글 저장
     * */
    @Override
    public Comment save(Comment comment) {
        commentMapper.save(comment);
        return comment;
    }
}
