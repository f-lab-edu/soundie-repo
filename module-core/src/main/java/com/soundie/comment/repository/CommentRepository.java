package com.soundie.comment.repository;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.domain.CommentWithAuthor;

import java.util.List;

public interface CommentRepository {

    List<Comment> findComments();

    List<CommentWithAuthor> findCommentsWithAuthorByPostId(Long postId);

    List<CommentWithAuthor> findCommentsWithAuthorByPostIdOrderByIdAsc(Long postId, Integer size);

    List<CommentWithAuthor> findCommentsWithAuthorByPostIdAndIdLessThanOrderByIdAsc(Long postId, Long cursor, Integer size);

    Number countCommentsByPostId(Long postId);

    Comment save(Comment comment);
}
