package com.soundie.post.domain;

import com.soundie.comment.domain.Comment;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
public class Post {

    private Long id;
    private Long memberId;
    private String title;
    private String artistName;
    private String musicPath;
    private String albumImgPath;
    private String albumName;
    private List<PostLike> likes = new ArrayList<>();
    private List<Comment> comments = new ArrayList<>();
    private LocalDateTime createdAt;

    public Post(Long memberId, String title, String artistName, String musicPath, String albumImgPath, String albumName) {
        this.memberId = memberId;
        this.title = title;
        this.artistName = artistName;
        this.musicPath = musicPath;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;
        this.createdAt = LocalDateTime.now();
    }

    public Post(Long id, Long memberId, String title, String artistName, String musicPath, String albumImgPath, String albumName, LocalDateTime createdAt) {
        this.id = id;
        this.memberId = memberId;
        this.title = title;
        this.artistName = artistName;
        this.musicPath = musicPath;
        this.albumImgPath = albumImgPath;
        this.albumName = albumName;
        this.createdAt = createdAt;
    }
}
