-- --------------------------------------------------------
-- 主机:                           192.168.95.233
-- 服务器版本:                        5.1.73 - Source distribution
-- 服务器操作系统:                      redhat-linux-gnu
-- HeidiSQL 版本:                  9.5.0.5196
-- --------------------------------------------------------

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET NAMES utf8 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


-- 导出 fiveTten 的数据库结构
CREATE DATABASE IF NOT EXISTS `fiveTten` /*!40100 DEFAULT CHARACTER SET latin1 */;
USE `fiveTten`;

-- 导出  表 fiveTten.recharge_log 结构
CREATE TABLE IF NOT EXISTS `recharge_log` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `unix` int(11) NOT NULL DEFAULT '0' COMMENT '时间戳',
  `channel` varchar(255) NOT NULL DEFAULT '""' COMMENT '来源',
  `order_id` varchar(255) NOT NULL DEFAULT '""' COMMENT '订单号',
  `app_id` varchar(255) NOT NULL DEFAULT '""' COMMENT '设备ID',
  `ext` text NOT NULL COMMENT 'INFO',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='充值记录';

-- 数据导出被取消选择。
/*!40101 SET SQL_MODE=IFNULL(@OLD_SQL_MODE, '') */;
/*!40014 SET FOREIGN_KEY_CHECKS=IF(@OLD_FOREIGN_KEY_CHECKS IS NULL, 1, @OLD_FOREIGN_KEY_CHECKS) */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
