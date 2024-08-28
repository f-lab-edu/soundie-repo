package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;

import java.util.List;

public interface CommentRepository {

    List<Comment> findComments();

    List<Comment> findCommentsByPostId(Long postId);

    Long countCommentsByPostId(Long postId);

    Comment save(Comment comment);
}
