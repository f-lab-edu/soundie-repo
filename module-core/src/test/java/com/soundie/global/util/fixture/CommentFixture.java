package com.soundie.global.util.fixture;

import com.soundie.comment.domain.Comment;
import com.soundie.member.domain.Member;
import com.soundie.post.domain.Post;

public class CommentFixture {

    public static final Comment createFirstComment(Member member, Post post) {
        Comment comment = new Comment(
                member.getId(),
                post.getId(),
                "댓글 내용1"
        );
        comment.setId(1L);
        return comment;
    }

    public static final Comment createSecondComment(Member member, Post post) {
        Comment comment = new Comment(
                member.getId(),
                post.getId(),
                "댓글 내용2"
        );
        comment.setId(2L);
        return comment;
    }
}
