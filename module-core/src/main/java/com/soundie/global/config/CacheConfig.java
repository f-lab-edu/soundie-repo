package com.soundie.global.config;

import com.soundie.global.common.util.CacheExpireTime;
import com.soundie.global.common.util.CacheNames;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;

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
        // 리소스 유형에 따라, 만료 시간을 다르게 지정
        RedisCacheConfiguration defaultCacheConfig = RedisCacheConfiguration.defaultCacheConfig()
                .serializeKeysWith(
                        RedisSerializationContext.SerializationPair.fromSerializer(new StringRedisSerializer())
                )
                .serializeValuesWith(
                        RedisSerializationContext.SerializationPair.fromSerializer(new GenericJackson2JsonRedisSerializer())
                );
        Map<String, RedisCacheConfiguration> redisCacheConfigMap = new HashMap<>();
        redisCacheConfigMap.put(CacheNames.POST, defaultCacheConfig.entryTtl(Duration.ofSeconds(CacheExpireTime.POST)));
        redisCacheConfigMap.put(CacheNames.COMMENT, defaultCacheConfig.entryTtl(Duration.ofHours(CacheExpireTime.COMMENT)));
        redisCacheConfigMap.put(CacheNames.COMMENT_COUNT, defaultCacheConfig.entryTtl(Duration.ofHours(CacheExpireTime.COMMENT_COUNT)));
        redisCacheConfigMap.put(CacheNames.LIKE_COUNT, defaultCacheConfig.entryTtl(Duration.ofHours(CacheExpireTime.LIKE_COUNT)));
        redisCacheConfigMap.put(CacheNames.MEMBER, defaultCacheConfig.entryTtl(Duration.ofSeconds(CacheExpireTime.MEMBER)));

        return RedisCacheManager.builder(redisCacheConnectionFactory())
                .withInitialCacheConfigurations(redisCacheConfigMap)
                .build();
    }

    @Bean(name = "redisCacheTemplate")
    public RedisTemplate<String, Object> redisCacheTemplate() {
        RedisTemplate<String, Object> redisTemplate = new RedisTemplate<>();
        redisTemplate.setKeySerializer(new StringRedisSerializer());
        redisTemplate.setValueSerializer(new GenericJackson2JsonRedisSerializer());
        redisTemplate.setConnectionFactory(redisCacheConnectionFactory());
        return redisTemplate;
    }
}
