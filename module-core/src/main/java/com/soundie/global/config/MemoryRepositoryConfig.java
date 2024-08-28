package com.soundie.global.config;

import com.soundie.chatRoom.repository.ChatRoomRepository;
import com.soundie.chatRoom.repository.MemoryChatRoomRepository;
import com.soundie.comment.repository.CommentRepository;
import com.soundie.comment.repository.MemoryCommentRepository;
import com.soundie.member.repository.MemberRepository;
import com.soundie.member.repository.MemoryMemberRepository;
import com.soundie.notification.repository.MemoryNotificationRepository;
import com.soundie.notification.repository.NotificationRepository;
import com.soundie.post.repository.MemoryPostLikeRepository;
import com.soundie.post.repository.MemoryPostRepository;
import com.soundie.post.repository.PostLikeRepository;
import com.soundie.post.repository.PostRepository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MemoryRepositoryConfig {

    @Bean
    public MemberRepository memberRepository(){
        return new MemoryMemberRepository();
    }

    @Bean
    public PostRepository postRepository(){
        return new MemoryPostRepository();
    }

    @Bean
    public PostLikeRepository postLikeRepository(){
        return new MemoryPostLikeRepository();
    }

    @Bean
    public CommentRepository commentRepository(){
        return new MemoryCommentRepository();
    }

    @Bean
    public ChatRoomRepository chatRoomRepository(){
        return new MemoryChatRoomRepository();
    }

    @Bean
    public NotificationRepository notificationRepository(){
        return new MemoryNotificationRepository();
    }
}
