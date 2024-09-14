package com.soundie.comment.mapper;

import com.soundie.comment.domain.Comment;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CommentMapper {

    List<Comment> findComments();

    List<Comment> findCommentsByPostId(@Param("postId") Long postId);

    List<Comment> findCommentsByPostIdOrderByIdAscCreatedAtAsc(
            @Param("postId") Long postId,
            @Param("size") Integer size);

    List<Comment> findCommentsByPostIdAndIdLessThanOrderByIdAscCreatedAtAsc(
            @Param("postId") Long postId,
            @Param("id") Long commentId,
            @Param("size") Integer size);

    Number countCommentsByPostId(@Param("postId") Long postId);

    void save(@Param("comment") Comment comment);
}
