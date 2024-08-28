package com.soundie.post.mapper;

import com.soundie.post.domain.Post;
import com.soundie.post.vo.PostVo;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface PostMapper {

    List<PostVo> findPosts();

    Optional<PostVo> findPostById(@Param("id") Long postId);

    void save(@Param("post") Post post);
}
