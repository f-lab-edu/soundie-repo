package com.soundie.post.domain;

import com.soundie.comment.domain.Comment;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class Post {

    private Long id;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private List<PostLike> likes = new ArrayList<>();
    private List<Comment> comments = new ArrayList<>();
    private String createdAt;

    public Post(String title, String artistName, String musicPath, String albumImgPath, String albumName, String createdAt) {
        this.title = title;
        this.artistName = artistName;
        this.musicPath = musicPath;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;

        // 수정 필요: 좋아요, 댓글
        this.likes.add(new PostLike());
        this.likes.add(new PostLike());
        this.comments.add(new Comment());
        this.comments.add(new Comment());

        this.createdAt = createdAt;
    }
}
