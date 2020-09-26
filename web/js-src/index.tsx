import ReactDOM from 'react-dom'
import React from 'react'
import MainController from './MainController'
import smoothscroll from 'smoothscroll-polyfill'

smoothscroll.polyfill();
window.addEventListener('load', e => ReactDOM.render(<MainController />, document.getElementById('main')));