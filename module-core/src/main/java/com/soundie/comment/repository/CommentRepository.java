package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;

import java.util.List;

public interface CommentRepository {

    List<Comment> findComments();

    List<Comment> findCommentsByPostId(Long postId);

    List<Comment> findCommentsByPostIdOrderByIdAsc(Long postId, Integer size);

    List<Comment> findCommentsByPostIdAndIdLessThanOrderByIdAsc(Long postId, Long cursor, Integer size);

    Number countCommentsByPostId(Long postId);

    Comment save(Comment comment);
}
