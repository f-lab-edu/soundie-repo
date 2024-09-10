package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;

import java.util.List;

public interface CommentRepository {

    List<Comment> findComments();

    List<Comment> findCommentsByPostId(Long postId);

    List<Comment> findCommentsByPostIdOrderByIdAscCreatedAtAsc(Long postId, Integer size);

    List<Comment> findCommentsByPostIdAndIdLessThanOrderByIdAscCreatedAtAsc(Long postId, Long cursor, Integer size);

    Long countCommentsByPostId(Long postId);

    Comment save(Comment comment);
}
