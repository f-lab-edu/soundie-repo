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
            @Param("member_id") Long memberId,
            @Param("post_id") Long postId);

    Long countPostLikesByPostId(@Param("post_id") Long postId);

    void save(@Param("postlike") PostLike postLike);

    void delete(@Param("postlike") PostLike postLike);

}
