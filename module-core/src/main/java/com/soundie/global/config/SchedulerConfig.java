package com.soundie.global.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

@EnableScheduling
@EnableAsync
@Configuration
public class SchedulerConfig {

    @Bean(name = "threadPoolTaskScheduler")
    public ThreadPoolTaskScheduler threadPoolTaskScheduler() {
        ThreadPoolTaskScheduler taskScheduler = new ThreadPoolTaskScheduler();
        taskScheduler.setPoolSize(Runtime.getRuntime().availableProcessors() * 2); // 스레드 수
        taskScheduler.setThreadNamePrefix("Scheduler-");
        taskScheduler.setWaitForTasksToCompleteOnShutdown(true); // 작업 완료까지 스프링종료 대기
        taskScheduler.initialize();
        return taskScheduler;
    }
}
