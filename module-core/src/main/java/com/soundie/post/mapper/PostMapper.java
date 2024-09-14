package com.soundie.post.mapper;

import com.soundie.post.domain.Post;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface PostMapper {

    List<Post> findPosts();

    List<Post> findPostsOrderByIdDesc(@Param("size") Integer size);

    List<Post> findPostsByIdLessThanOrderByIdDesc(
            @Param("id") Long postId,
            @Param("size") Integer size);

    Optional<Post> findPostById(@Param("id") Long postId);

    void save(@Param("post") Post post);
}
