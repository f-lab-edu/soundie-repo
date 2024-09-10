package com.soundie.global.config;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;

@Configuration
@RequiredArgsConstructor
public class CacheConfig {

    @Value("${spring.redis.cache.host}")
    private String host;

    @Value("${spring.redis.cache.port}")
    private int port;

    @Bean(name = "redisCacheConnectionFactory")
    public RedisConnectionFactory redisCacheConnectionFactory() {
        RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
        redisStandaloneConfiguration.setHostName(host);
        redisStandaloneConfiguration.setPort(port);
        return new LettuceConnectionFactory(redisStandaloneConfiguration);
    }

    @Bean
    public RedisCacheManager cacheManager() {
        // RedisCacheManager 관련 옵션
        // 1. serializeKeysWith : Key Serializer 기본 값은 StringRedisSerializer 로 지정
        // 2. serializeValuesWith : 캐시 Value 를 직렬화-역직렬화 하는데 사용하는 Pair 지정
        // Value 는 다양한 자료구조가 올 수 있기 때문에 GenericJackson2JsonRedisSerializer 를 사용
        RedisCacheConfiguration defaultCacheConfig = RedisCacheConfiguration.defaultCacheConfig()
                .serializeValuesWith(
                        RedisSerializationContext.SerializationPair.fromSerializer(new GenericJackson2JsonRedisSerializer())
                );

        // 3. 리소스 유형에 따라 만료 시간을 다르게 지정
        Map<String, RedisCacheConfiguration> redisCacheConfigMap = new HashMap<>();
        redisCacheConfigMap.put("post", defaultCacheConfig.entryTtl(Duration.ofHours(1)));
        redisCacheConfigMap.put("comment", defaultCacheConfig.entryTtl(Duration.ofHours(1)));

        RedisCacheManager redisCacheManager = RedisCacheManager.builder(redisCacheConnectionFactory())
                .withInitialCacheConfigurations(redisCacheConfigMap)
                .build();

        return redisCacheManager;
    }
}
