<?php


namespace wn;

require_once __DIR__.'/../vendor/autoload.php';

$content = file_get_contents("/home/jean/.whatnext.conf");
$lines = explode("\n", $content);

$pipeExploder = function($entry) {
    return explode('|', $entry);
};

$notEmpty = function($a) {
    return \f\op\not(\f\op\isEmpty($a));
};
$validLines = array_filter($lines, $notEmpty);

print_r($validLines);
