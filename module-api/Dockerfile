# 1. Base image
FROM openjdk:17-jdk-slim AS builder

# 2. Set working directory
WORKDIR /app

# 3. Copy Gradle wrapper and setting files to cache dependencies
COPY gradlew /app/
COPY gradle /app/gradle/
COPY build.gradle /app/
COPY settings.gradle /app/

COPY module-core/build.gradle /app/module-core/
COPY module-core/src /app/module-core/src
COPY module-api/build.gradle /app/module-api/
COPY module-api/src /app/module-api/src

# 4. Ensure gradlew is executable
RUN chmod +x gradlew

# 5. Build the application
# Download dependencies (caches dependencies to avoid re-downloading)
RUN ./gradlew dependencies --no-daemon || return 0
RUN ./gradlew :module-api:build -x test --stacktrace

# 6. Production image (only copies JAR file)
FROM openjdk:17-jdk-slim
WORKDIR /app
COPY --from=builder /app/module-api/build/libs/module-api-0.0.1-SNAPSHOT.jar /module-api.jar

# 7. Expose port
EXPOSE 8080

# 8. Run the application with a profile environment variable
# Override in docker-compose
ENV SPRING_PROFILES_ACTIVE=local
ENTRYPOINT ["java", "-Dspring.profiles.active=${SPRING_PROFILES_ACTIVE}", "-jar", "/module-api.jar"]
