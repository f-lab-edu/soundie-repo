package com.soundie.comment.mapper;

import com.soundie.comment.domain.Comment;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface CommentMapper {

    List<Comment> findComments();

    List<Comment> findCommentsByPostId(@Param("postId") Long postId);

    Long countCommentsByPostId(@Param("postId") Long postId);

    void save(@Param("comment") Comment comment);
}
