package com.soundie.comment.mapper;

import com.soundie.comment.domain.Comment;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface CommentMapper {

    List<Comment> findComments();

    List<Comment> findCommentsByPostId(@Param("post_id") Long postId);

    void save(@Param("comment") Comment comment);
}
