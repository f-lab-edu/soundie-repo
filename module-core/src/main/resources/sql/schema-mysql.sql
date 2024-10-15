SET foreign_key_checks = 0; -- 외래키 체크 해제

drop table if exists member CASCADE;
create table member
(
		 id bigint AUTO_INCREMENT,
         name VARCHAR(255),
         created_at  TIMESTAMP(6) DEFAULT CURRENT_TIMESTAMP(6),
		 primary key (id)
) comment = "회원" -- 테이블 설명
  default charset = utf8mb4 -- mb4는 이모티콘도 넣을 수 있음
  engine = InnoDB; -- 엔진 설정

drop table if exists post CASCADE;
create table post
(
		 id bigint AUTO_INCREMENT,
         member_id bigint,
         title varchar(255),
         artist_name varchar(255),
         music_path varchar(1000),
         album_img_path varchar(1000),
         album_name varchar(1000),
         created_at  TIMESTAMP(6) DEFAULT CURRENT_TIMESTAMP(6),
		 primary key (id),
		 foreign key (member_id) references member(id)
		    on delete cascade
		    on update cascade
) comment = "음원 게시물" -- 테이블 설명
  default charset = utf8mb4 -- mb4는 이모티콘도 넣을 수 있음
  engine = InnoDB; -- 엔진 설정
create index post_idx_id on post(id);

drop table if exists comment CASCADE;
create table comment
(
		 id bigint AUTO_INCREMENT,
         member_id bigint,
         post_id bigint,
         content varchar(1000),
         created_at  TIMESTAMP(6) DEFAULT CURRENT_TIMESTAMP(6),
		 primary key (id),
		 foreign key (member_id) references member(id)
		    on delete cascade
		    on update cascade,
		 foreign key (post_id) references post(id)
		    on delete cascade
		    on update cascade
) comment = "음원 게시물 댓글" -- 테이블 설명
  default charset = utf8mb4 -- mb4는 이모티콘도 넣을 수 있음
  engine = InnoDB; -- 엔진 설정
create index comment_idx_id on comment(id);

drop table if exists postlike CASCADE;
create table postlike
(
		 id bigint AUTO_INCREMENT,
         member_id bigint,
         post_id bigint,
         created_at  TIMESTAMP(6) DEFAULT CURRENT_TIMESTAMP(6),
		 primary key (id),
		 foreign key (member_id) references member(id)
		    on delete cascade
		    on update cascade,
		 foreign key (post_id) references post(id)
		    on delete cascade
		    on update cascade
) comment = "음원 게시물 좋아요" -- 테이블 설명
  default charset = utf8mb4 -- mb4는 이모티콘도 넣을 수 있음
  engine = InnoDB; -- 엔진 설정

drop table if exists chatroom CASCADE;
create table chatroom
(
		 id bigint AUTO_INCREMENT,
         host_member_id bigint,
         guest_member_id bigint,
         name varchar(255),
         description varchar(1000),
         created_at  TIMESTAMP(6) DEFAULT CURRENT_TIMESTAMP(6),
		 primary key (id),
		 foreign key (host_member_id) references member(id)
		    on delete cascade
		    on update cascade,
		 foreign key (guest_member_id) references member(id)
		    on delete cascade
		    on update cascade
) comment = "채팅 방" -- 테이블 설명
  default charset = utf8mb4 -- mb4는 이모티콘도 넣을 수 있음
  engine = InnoDB; -- 엔진 설정

drop table if exists chatmessage CASCADE;
create table chatmessage
(
		 id bigint AUTO_INCREMENT,
         chatroom_id bigint,
         sender_id bigint,
         type_name varchar(255),
         content varchar(1000),
         content_img_path varchar(1000),
         member_cnt tinyint,
         created_at  TIMESTAMP(6) DEFAULT CURRENT_TIMESTAMP(6),
		 primary key (id),
		 foreign key (chatroom_id) references chatroom(id)
		    on delete cascade
		    on update cascade,
		 foreign key (sender_id) references member(id)
		    on delete cascade
		    on update cascade
) comment = "채팅 메시지" -- 테이블 설명
  default charset = utf8mb4 -- mb4는 이모티콘도 넣을 수 있음
  engine = InnoDB; -- 엔진 설정
create index chatmessage_idx_id on chatmessage(id);

SET foreign_key_checks = 1; -- 외래키 체크 설정
