package com.soundie.post.mapper;

import com.soundie.post.domain.Post;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface PostMapper {

    List<Post> findPosts();

    List<Post> findPostsByOrderByIdDescCreatedAtDesc(Integer size);

    List<Post> findPostsByIdLessThanOrderByIdDescCreatedAtDesc(@Param("id") Long postId,
                                                               Integer size);

    Optional<Post> findPostById(@Param("id") Long postId);

    void save(@Param("post") Post post);
}
