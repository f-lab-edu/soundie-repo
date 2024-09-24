package com.soundie.global.util.fixture;

import com.soundie.comment.domain.Comment;
import com.soundie.comment.domain.CommentWithAuthor;
import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;

import java.time.LocalDateTime;

public class CommentFixture {

    public static final Comment createFirstComment(Member member, Post post) {
        return new Comment(
                1L,
                member.getId(),
                post.getId(),
                "댓글 내용1"
        );
    }

    public static final CommentWithAuthor createFirstCommentWithAuthor(Member member, Post post) {
        return new CommentWithAuthor(
                1L,
                member.getId(),
                member.getName(),
                post.getId(),
                "댓글 내용1",
                LocalDateTime.now()
        );
    }

    public static final CommentWithAuthor createSecondCommentWithAuthor(Member member, Post post) {
        return new CommentWithAuthor(
                2L,
                member.getId(),
                member.getName(),
                post.getId(),
                "댓글 내용2",
                LocalDateTime.now()
        );
    }
}
