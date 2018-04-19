create database if not exists zbnotice;
use zbnotice;

create table if not exists `content` (
    `zone_id` bigint(20) NOT NULL,
    `zone_cont` varchar(8192) NOT NULL DEFAULT "",
    PRIMARY KEY (`zone_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

create table if not exists `iptable` (
    `ip` varchar(128) NOT NULL,
    PRIMARY KEY (`ip`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

