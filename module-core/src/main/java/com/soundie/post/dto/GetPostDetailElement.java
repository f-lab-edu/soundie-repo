package com.soundie.post.dto;

import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;
import com.soundie.post.domain.PostLike;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Builder
public class GetPostDetailElement {

    private Long postId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private Number likeCount;
    private Number commentCount;
    private LocalDateTime createdAt;
    private Boolean liked;

    public static GetPostDetailElement of(Post post, Member member) {
        Boolean liked = isLike(post, member);

        return GetPostDetailElement.builder()
                .postId(post.getId())
                .title(post.getTitle())
                .artistName(post.getArtistName())
                .musicPath(post.getMusicPath())
                .albumImgPath(post.getAlbumImgPath())
                .albumName(post.getAlbumName())
                .likeCount(post.getLikes().size())
                .commentCount(post.getComments().size())
                .createdAt(post.getCreatedAt())
                .liked(liked)
                .build();
    }

    private static Boolean isLike(Post post, Member member) {
        if (member == null){
            return false;
        }

        List<PostLike> likes = post.getLikes();
        Long memberId = member.getId();

        for (PostLike like : likes){
            if (like.getMemberId().equals(memberId)){
                return true;
            }
        }
        return false;
    }
}
