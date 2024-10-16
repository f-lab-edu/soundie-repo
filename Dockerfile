# 1. Docker base image를 지정
FROM openjdk:17-alpine

# 2. Docker Container의 8080포트를 개방
EXPOSE 8080

# 3. 지정된 경로의 jar 파일을, Docker image의 module-api.jar로 복사
ARG JAR_FILE=build/libs/*.jar
COPY ${JAR_FILE} module-api.jar

# 4. Docker Container가 시작될때, 실행할 명령어
ARG SPRING_PROFILE=prod
ENTRYPOINT ["java", "-Dspring.profiles.active=${SPRING_PROFILE}", "-jar", "/module-api.jar"]
