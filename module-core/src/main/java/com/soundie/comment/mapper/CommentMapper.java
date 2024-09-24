package com.soundie.comment.mapper;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.domain.CommentWithAuthor;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CommentMapper {

    List<Comment> findComments();

    List<CommentWithAuthor> findCommentsWithAuthorByPostId(@Param("postId") Long postId);

    List<CommentWithAuthor> findCommentsWithAuthorByPostIdOrderByIdAsc(
            @Param("postId") Long postId,
            @Param("size") Integer size);

    List<CommentWithAuthor> findCommentsWithAuthorByPostIdAndIdLessThanOrderByIdAsc(
            @Param("postId") Long postId,
            @Param("id") Long commentId,
            @Param("size") Integer size);

    Number countCommentsByPostId(@Param("postId") Long postId);

    void save(@Param("comment") Comment comment);
}
