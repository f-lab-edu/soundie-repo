package com.soundie;

import org.springframework.stereotype.Component;

@Component
public class TestBean {
    public void dependencyTest() {
        System.out.println("성공적으로 로딩됐습니다.");
    }
}
