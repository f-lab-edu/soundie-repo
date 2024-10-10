package com.soundie.post.mapper;

import com.soundie.post.domain.PostLike;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface PostLikeMapper {

    List<PostLike> findPostLikes();

    Optional<PostLike> findPostLikeByMemberIdAndPostId(
            @Param("memberId") Long memberId,
            @Param("postId") Long postId);

    Number countPostLikesByPostId(@Param("postId") Long postId);

    void save(@Param("postLike") PostLike postLike);

    void saveAll(@Param("postLikes") List<PostLike> postLikes);

    void delete(@Param("postLike") PostLike postLike);
}
