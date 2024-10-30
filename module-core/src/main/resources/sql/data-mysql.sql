insert into member (name) values ('admin');
insert into member (name) values ('회원2');

insert into post (member_id, title, artist_name, music_path, album_img_path, album_name)
values (1, '노래 제목1', '회원2', '~', '~~', '앨범 제목1');
