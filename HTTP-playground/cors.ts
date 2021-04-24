import express = require('express');
import path = require('path');

const port80 = express();
port80.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'index.html'));
});

port80.listen(80, () => {
  console.log('listen at 80');
});

port80.get('/index.js', (req, res) => {
  res.sendFile(path.join(__dirname, 'index.js'));
});

const port81 = express();
port81.get('/', (req, res) => {
  res.send('hello');
});

port81.listen(81, () => {
  console.log('listen at 81');
});
