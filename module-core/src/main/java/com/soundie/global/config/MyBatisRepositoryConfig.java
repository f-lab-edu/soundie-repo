package com.soundie.global.config;

import com.soundie.chatRoom.repository.ChatRoomRepository;
import com.soundie.chatRoom.repository.MemoryChatRoomRepository;
import com.soundie.comment.mapper.CommentMapper;
import com.soundie.comment.repository.CommentRepository;
import com.soundie.comment.repository.MyBatisCommentRepository;
import com.soundie.member.mapper.MemberMapper;
import com.soundie.member.repository.MemberRepository;
import com.soundie.member.repository.MyBatisMemberRepository;
import com.soundie.notification.repository.MemoryNotificationRepository;
import com.soundie.notification.repository.NotificationRepository;
import com.soundie.post.mapper.PostLikeMapper;
import com.soundie.post.mapper.PostMapper;
import com.soundie.post.repository.MyBatisPostLikeRepository;
import com.soundie.post.repository.MyBatisPostRepository;
import com.soundie.post.repository.PostLikeRepository;
import com.soundie.post.repository.PostRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class MyBatisRepositoryConfig {

    private final MemberMapper memberMapper;
    private final PostMapper postMapper;
    private final PostLikeMapper postLikeMapper;
    private final CommentMapper commentMapper;


    @Bean
    public MemberRepository memberRepository(){
        return new MyBatisMemberRepository(memberMapper);
    }

    @Bean
    public PostRepository postRepository(){
        return new MyBatisPostRepository(postMapper);
    }

    @Bean
    public PostLikeRepository postLikeRepository(){
        return new MyBatisPostLikeRepository(postLikeMapper);
    }

    @Bean
    public CommentRepository commentRepository(){
        return new MyBatisCommentRepository(commentMapper);
    }

    /*
    * 임시로 MemoryRepository Bean 사용
    * */
    @Bean
    public ChatRoomRepository chatRoomRepository(){
        return new MemoryChatRoomRepository();
    }

    @Bean
    public NotificationRepository notificationRepository(){
        return new MemoryNotificationRepository();
    }
}
