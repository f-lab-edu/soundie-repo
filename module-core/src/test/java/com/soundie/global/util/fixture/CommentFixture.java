package com.soundie.global.util.fixture;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;

public class CommentFixture {

    public static final Comment createFirstComment(Member member, Post post) {
        return new Comment(
                1L,
                member.getId(),
                post.getId(),
                "댓글 내용1"
        );
    }

    public static final Comment createSecondComment(Member member, Post post) {
        return new Comment(
                2L,
                member.getId(),
                post.getId(),
                "댓글 내용2"
        );
    }
}
