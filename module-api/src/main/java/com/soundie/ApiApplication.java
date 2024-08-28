package com.soundie;

import com.soundie.global.config.Config;
import com.soundie.global.config.MyBatisRepositoryConfig;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Import;

@Import({Config.class, MyBatisRepositoryConfig.class})
@SpringBootApplication(scanBasePackages = {"com.soundie.api"})
public class ApiApplication {
    public static void main(String[] args) {
        SpringApplication.run(ApiApplication.class, args);
    }
}
