package com.soundie.global.config;

import com.soundie.chatRoom.service.ChatRoomService;
import com.soundie.comment.service.CommentService;
import com.soundie.member.service.MemberService;
import com.soundie.notification.service.NotificationService;
import com.soundie.post.service.PostService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Config {

    @Autowired
    MemoryRepositoryConfig repositoryConfig;

    @Bean
    public MemberService memberService(){
        return new MemberService(
                repositoryConfig.memberRepository()
        );
    }

    @Bean
    public PostService postService(){
        return new PostService(
                repositoryConfig.postRepository(),
                repositoryConfig.postLikeRepository(),
                repositoryConfig.memberRepository()
        );
    }

    @Bean
    public CommentService commentService(){
        return new CommentService(
                repositoryConfig.commentRepository(),
                repositoryConfig.memberRepository(),
                repositoryConfig.postRepository()
        );
    }

    @Bean
    public ChatRoomService chatRoomService(){
        return new ChatRoomService(
                repositoryConfig.chatRoomRepository()
        );
    }

    @Bean
    public NotificationService notificationService(){
        return new NotificationService(
                repositoryConfig.notificationRepository()
        );
    }
}
