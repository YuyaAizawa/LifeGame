!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function t(n){return r(2,n,function(r){return function(t){return n(r,t)}})}function e(n){return r(3,n,function(r){return function(t){return function(e){return n(r,t,e)}}})}function u(n){return r(4,n,function(r){return function(t){return function(e){return function(u){return n(r,t,e,u)}}}})}function i(n){return r(5,n,function(r){return function(t){return function(e){return function(u){return function(i){return n(r,t,e,u,i)}}}}})}function f(n){return r(6,n,function(r){return function(t){return function(e){return function(u){return function(i){return function(f){return n(r,t,e,u,i,f)}}}}}})}function a(n){return r(7,n,function(r){return function(t){return function(e){return function(u){return function(i){return function(f){return function(a){return n(r,t,e,u,i,f,a)}}}}}}})}function o(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function c(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function v(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function s(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}function b(n,r,t,e,u,i,f){return 6===n.a?n.f(r,t,e,u,i,f):n(r)(t)(e)(u)(i)(f)}function l(n,r,t,e,u,i,f,a){return 7===n.a?n.f(r,t,e,u,i,f,a):n(r)(t)(e)(u)(i)(f)(a)}var d={$:0};function h(n,r){return{$:1,a:n,b:r}}var $=t(h);function g(n){for(var r=d,t=n.length;t--;)r=h(n[t],r);return r}var p=e(function(n,r,t){for(var e=[];r.b&&t.b;r=r.b,t=t.b)e.push(o(n,r.a,t.a));return g(e)});function m(n,r){for(var t,e=[],u=y(n,r,0,e);u&&(t=e.pop());u=y(t.a,t.b,0,e));return u}function y(n,r,t,e){if(t>100)return e.push(_(n,r)),!0;if(n===r)return!0;if("object"!=typeof n||null===n||null===r)return"function"==typeof n&&q(5),!1;for(var u in 0>n.$&&(n=_r(n),r=_r(r)),n)if(!y(n[u],r[u],t+1,e))return!1;return!0}function w(n,r,t){if("object"!=typeof n)return n===r?0:r>n?-1:1;if(void 0===n.$)return(t=w(n.a,r.a))?t:(t=w(n.b,r.b))?t:w(n.c,r.c);for(;n.b&&r.b&&!(t=w(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var A=t(function(n,r){var t=w(n,r);return 0>t?wr:t?yr:mr}),F=0;function _(n,r){return{a:n,b:r}}function j(n,r,t){return{a:n,b:r,c:t}}function k(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}var E=e(function(n,r,t){for(var e=Array(n),u=0;n>u;u++)e[u]=t(r+u);return e}),N=t(function(n,r){for(var t=Array(n),e=0;n>e&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,_(t,r)}),T=t(function(n,r){return r[n]}),L=e(function(n,r,t){for(var e=t.length,u=Array(e),i=0;e>i;i++)u[i]=t[i];return u[n]=r,u}),S=e(function(n,r,t){for(var e=t.length,u=0;e>u;u++)r=o(n,t[u],r);return r}),x=e(function(n,r,t){for(var e=t.length-1;e>=0;e--)r=o(n,t[e],r);return r}),C=e(function(n,r,t){for(var e=t.length,u=Array(e),i=0;e>i;i++)u[i]=o(n,r+i,t[i]);return u});function q(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var B=t(function(n,r){var t=r%n;return 0===n?q(11):t>0&&0>n||0>t&&n>0?t+n:t});var O=Math.ceil,R=Math.floor,Q=Math.log;function z(n){return{$:2,b:n}}var I=z(function(n){return"number"!=typeof n?tn("an INT",n):n>-2147483647&&2147483647>n&&(0|n)===n?ct(n):!isFinite(n)||n%1?tn("an INT",n):ct(n)}),Y=z(function(n){return"boolean"==typeof n?ct(n):tn("a BOOL",n)}),D=z(function(n){return"number"==typeof n?ct(n):tn("a FLOAT",n)});z(function(n){return ct(fn(n))}),z(function(n){return"string"==typeof n?ct(n):n instanceof String?ct(n+""):tn("a STRING",n)});var G=t(function(n,r){return{$:6,d:n,b:r}});function P(n,r){return{$:9,f:n,g:r}}var X=t(function(n,r){return{$:10,b:r,h:n}});var K=t(function(n,r){return P(n,[r])}),M=e(function(n,r,t){return P(n,[r,t])}),J=u(function(n,r,t,e){return P(n,[r,t,e])}),W=i(function(n,r,t,e,u){return P(n,[r,t,e,u])}),H=a(function(n,r,t,e,u,i,f){return P(n,[r,t,e,u,i,f])}),U=t(function(n,r){return V(n,an(r))});function V(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?ct(n.c):tn("null",r);case 3:return nn(r)?Z(n.b,r,g):tn("a LIST",r);case 4:return nn(r)?Z(n.b,r,rn):tn("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return tn("an OBJECT with a field named `"+t+"`",r);var e=V(n.b,r[t]);return tt(e)?e:ot(o(st,t,e.a));case 7:var u=n.e;if(!nn(r))return tn("an ARRAY",r);if(u>=r.length)return tn("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=V(n.b,r[u]);return tt(e)?e:ot(o(bt,u,e.a));case 8:if("object"!=typeof r||null===r||nn(r))return tn("an OBJECT",r);var i=d;for(var f in r)if(r.hasOwnProperty(f)){e=V(n.b,r[f]);if(!tt(e))return ot(o(st,f,e.a));i=h(_(f,e.a),i)}return ct(Rr(i));case 9:for(var a=n.f,c=n.g,v=0;c.length>v;v++){e=V(c[v],r);if(!tt(e))return e;a=a(e.a)}return ct(a);case 10:e=V(n.b,r);return tt(e)?V(n.h(e.a),r):e;case 11:for(var s=d,b=n.g;b.b;b=b.b){e=V(b.a,r);if(tt(e))return e;s=h(e.a,s)}return ot(lt(Rr(s)));case 1:return ot(o(vt,n.a,fn(r)));case 0:return ct(n.a)}}function Z(n,r,t){for(var e=r.length,u=Array(e),i=0;e>i;i++){var f=V(n,r[i]);if(!tt(f))return ot(o(bt,i,f.a));u[i]=f.a}return ct(t(u))}function nn(n){return Array.isArray(n)||"function"==typeof FileList&&n instanceof FileList}function rn(n){return o(it,n.length,function(r){return n[r]})}function tn(n,r){return ot(o(vt,"Expecting "+n,fn(r)))}function en(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return en(n.b,r.b);case 6:return n.d===r.d&&en(n.b,r.b);case 7:return n.e===r.e&&en(n.b,r.b);case 9:return n.f===r.f&&un(n.g,r.g);case 10:return n.h===r.h&&en(n.b,r.b);case 11:return un(n.g,r.g)}}function un(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;t>e;e++)if(!en(n[e],r[e]))return!1;return!0}function fn(n){return n}function an(n){return n}fn(null);function on(n){return{$:0,a:n}}function cn(n){return{$:2,b:n,c:null}}var vn=t(function(n,r){return{$:3,b:n,d:r}});var sn=0;function bn(n){var r={$:0,e:sn++,f:n,g:null,h:[]};return pn(r),r}function ln(n){return cn(function(r){r(on(bn(n)))})}function dn(n,r){n.h.push(r),pn(n)}var hn=t(function(n,r){return cn(function(t){dn(n,r),t(on(F))})});var $n=!1,gn=[];function pn(n){if(gn.push(n),!$n){for($n=!0;n=gn.shift();)mn(n);$n=!1}}function mn(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b(function(r){n.f=r,pn(n)}));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function yn(n,r,t,e,u,i){var f=o(U,n,fn(r?r.flags:void 0));tt(f)||q(2);var a={},c=(f=t(f.a)).a,v=i(b,c),s=function(n,r){var t;for(var e in wn){var u=wn[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=Fn(u,r)}return t}(a,b);function b(n,r){v(c=(f=o(e,n,c)).a,r),Nn(a,f.b,u(c))}return Nn(a,f.b,u(c)),s?{ports:s}:{}}var wn={};function An(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function Fn(n,r){var t={g:r,h:void 0},e=n.c,u=n.d,i=n.e,f=n.f;return t.h=bn(o(vn,function n(r){return o(vn,n,{$:5,b:function(n){var a=n.a;return 0===n.$?c(u,t,a,r):i&&f?v(e,t,a.i,a.j,r):c(e,t,i?a.i:a.j,r)}})},n.b))}var _n=t(function(n,r){return cn(function(t){n.g(r),t(on(F))})}),jn=t(function(n,r){return o(hn,n.h,{$:0,a:r})});function kn(n){return function(r){return{$:1,k:n,l:r}}}function En(n){return{$:2,m:n}}function Nn(n,r,t){var e={};for(var u in Tn(!0,r,e,null),Tn(!1,t,e,null),n)dn(n[u],{$:"fx",a:e[u]||{i:d,j:d}})}function Tn(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){return o(n?wn[r].e:wn[r].f,function(n){for(var r=t;r;r=r.q)n=r.p(n);return n},e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:d,j:d},n?t.i=h(r,t.i):t.j=h(r,t.j),t}(n,i,t[u]));case 2:for(var f=r.m;f.b;f=f.b)Tn(n,f.a,t,e);return;case 3:return void Tn(n,r.o,t,{p:r.n,q:e})}}var Ln=t(function(n,r){return cn(function(){var t=setInterval(function(){bn(r)},n);return function(){clearInterval(t)}})});var Sn,xn="undefined"!=typeof document?document:{};function Cn(n,r){n.appendChild(r)}function qn(n){return{$:0,a:n}}var Bn=t(function(n,r){return t(function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b||0,u.push(f)}return i+=u.length,{$:1,c:r,d:Yn(t),e:u,f:n,b:i}})}),On=Bn(void 0);t(function(n,r){return t(function(t,e){for(var u=[],i=0;e.b;e=e.b){var f=e.a;i+=f.b.b||0,u.push(f)}return i+=u.length,{$:2,c:r,d:Yn(t),e:u,f:n,b:i}})})(void 0);var Rn=t(function(n,r){return{$:"a0",n:n,o:r}}),Qn=t(function(n,r){return{$:"a2",n:n,o:r}}),zn=t(function(n,r){return{$:"a3",n:n,o:r}});var In;function Yn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var f=r[e]||(r[e]={});"a3"===e&&"class"===u?Dn(f,u,i):f[u]=i}else"className"===u?Dn(r,u,an(i)):r[u]=an(i)}return r}function Dn(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function Gn(n,r){var t=n.$;if(5===t)return Gn(n.k||(n.k=n.m()),r);if(0===t)return xn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(f=Gn(e,i)).elm_event_node_ref=i,f}if(3===t)return Pn(f=n.h(n.g),r,n.d),f;var f=n.f?xn.createElementNS(n.f,n.c):xn.createElement(n.c);Sn&&"a"==n.c&&f.addEventListener("click",Sn(f)),Pn(f,r,n.d);for(var a=n.e,o=0;a.length>o;o++)Cn(f,Gn(1===t?a[o]:a[o].b,r));return f}function Pn(n,r,t){for(var e in t){var u=t[e];"a1"===e?Xn(n,u):"a0"===e?Jn(n,r,u):"a3"===e?Kn(n,u):"a4"===e?Mn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function Xn(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Kn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Mn(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function Jn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],f=e[u];if(i){if(f){if(f.q.$===i.$){f.q=i;continue}n.removeEventListener(u,f)}f=Wn(r,i),n.addEventListener(u,f,In&&{passive:2>Le(i)}),e[u]=f}else n.removeEventListener(u,f),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){In=!0}}))}catch(n){}function Wn(n,r){function t(r){var e=t.q,u=V(e.a,r);if(tt(u)){for(var i,f=Le(e),a=u.a,o=f?3>f?a.a:a.l:a,c=1==f?a.b:3==f&&a.S,v=(c&&r.stopPropagation(),(2==f?a.b:3==f&&a.Q)&&r.preventDefault(),n);i=v.j;){if("function"==typeof i)o=i(o);else for(var s=i.length;s--;)o=i[s](o);v=v.p}v(o,c)}}return t.q=r,t}function Hn(n,r){return n.$==r.$&&en(n.a,r.a)}function Un(n,r){var t=[];return Zn(n,r,t,0),t}function Vn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Zn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Vn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;t>u;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var f=n.l,a=r.l,o=f.length,c=o===a.length;c&&o--;)c=f[o]===a[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Zn(n.k,r.k,v,0),void(v.length>0&&Vn(t,1,e,v));case 4:for(var s=n.j,b=r.j,l=!1,d=n.k;4===d.$;)l=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)l=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return l&&s.length!==b.length?void Vn(t,0,e,r):((l?function(n,r){for(var t=0;n.length>t;t++)if(n[t]!==r[t])return!1;return!0}(s,b):s===b)||Vn(t,2,e,b),void Zn(d,h,t,e+1));case 0:return void(n.a!==r.a&&Vn(t,3,e,r.a));case 1:return void nr(n,r,t,e,tr);case 2:return void nr(n,r,t,e,er);case 3:if(n.h!==r.h)return void Vn(t,0,e,r);var $=rr(n.d,r.d);$&&Vn(t,4,e,$);var g=r.i(n.g,r.g);return void(g&&Vn(t,5,e,g))}}}function nr(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=rr(n.d,r.d);i&&Vn(t,4,e,i),u(n,r,t,e)}else Vn(t,0,e,r)}function rr(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],f=r[u];i===f&&"value"!==u&&"checked"!==u||"a0"===t&&Hn(i,f)||((e=e||{})[u]=f)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var a=rr(n[u],r[u]||{},u);a&&((e=e||{})[u]=a)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function tr(n,r,t,e){var u=n.e,i=r.e,f=u.length,a=i.length;f>a?Vn(t,6,e,{v:a,i:f-a}):a>f&&Vn(t,7,e,{v:f,e:i});for(var o=a>f?f:a,c=0;o>c;c++){var v=u[c];Zn(v,i[c],t,++e),e+=v.b||0}}function er(n,r,t,e){for(var u=[],i={},f=[],a=n.e,o=r.e,c=a.length,v=o.length,s=0,b=0,l=e;c>s&&v>b;){var d=(k=a[s]).a,h=(E=o[b]).a,$=k.b,g=E.b,p=void 0,m=void 0;if(d!==h){var y=a[s+1],w=o[b+1];if(y){var A=y.a,F=y.b;m=h===A}if(w){var _=w.a,j=w.b;p=d===_}if(p&&m)Zn($,j,u,++l),ir(i,u,d,g,b,f),l+=$.b||0,fr(i,u,d,F,++l),l+=F.b||0,s+=2,b+=2;else if(p)l++,ir(i,u,h,g,b,f),Zn($,j,u,l),l+=$.b||0,s+=1,b+=2;else if(m)fr(i,u,d,$,++l),l+=$.b||0,Zn(F,g,u,++l),l+=F.b||0,s+=2,b+=1;else{if(!y||A!==_)break;fr(i,u,d,$,++l),ir(i,u,h,g,b,f),l+=$.b||0,Zn(F,j,u,++l),l+=F.b||0,s+=2,b+=2}}else Zn($,g,u,++l),l+=$.b||0,s++,b++}for(;c>s;){var k;fr(i,u,(k=a[s]).a,$=k.b,++l),l+=$.b||0,s++}for(;v>b;){var E,N=N||[];ir(i,u,(E=o[b]).a,E.b,void 0,N),b++}(u.length>0||f.length>0||N)&&Vn(t,8,e,{w:u,x:f,y:N})}var ur="_elmW6BL";function ir(n,r,t,e,u,i){var f=n[t];if(!f)return i.push({r:u,A:f={c:0,z:e,r:u,s:void 0}}),void(n[t]=f);if(1===f.c){i.push({r:u,A:f}),f.c=2;var a=[];return Zn(f.z,e,a,f.r),f.r=u,void(f.s.s={w:a,A:f})}ir(n,r,t+ur,e,u,i)}function fr(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var f=[];return Zn(e,i.z,f,u),void Vn(r,9,u,{w:f,A:i})}fr(n,r,t+ur,e,u)}else{var a=Vn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:a}}}function ar(n,r,t,e){!function n(r,t,e,u,i,f,a){var o=e[u];var c=o.r;for(;c===i;){var v=o.$;if(1===v)ar(r,t.k,o.s,a);else if(8===v){o.t=r,o.u=a;var s=o.s.w;s.length>0&&n(r,t,s,0,i,f,a)}else if(9===v){o.t=r,o.u=a;var b=o.s;if(b){b.A.s=r;var s=b.w;s.length>0&&n(r,t,s,0,i,f,a)}}else o.t=r,o.u=a;if(!(o=e[++u])||(c=o.r)>f)return u}var l=t.$;if(4===l){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,i+1,f,r.elm_event_node_ref)}var h=t.e;var $=r.childNodes;for(var g=0;h.length>g;g++){var p=1===l?h[g]:h[g].b,m=++i+(p.b||0);if(c>=i&&m>=c&&(u=n($[g],p,e,u,i,m,a),!(o=e[u])||(c=o.r)>f))return u;i=m}return u}(n,r,t,0,0,r.b,e)}function or(n,r,t,e){return 0===t.length?n:(ar(n,r,t,e),cr(n,t))}function cr(n,r){for(var t=0;r.length>t;t++){var e=r[t],u=e.t,i=vr(u,e);u===n&&(n=i)}return n}function vr(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=Gn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Pn(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return cr(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;t.i>e;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];u.length>e;e++)n.insertBefore(Gn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var f=t.A;return void 0!==f.r&&n.parentNode.removeChild(n),f.s=cr(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=xn.createDocumentFragment(),e=0;n.length>e;e++){var u=n[e],i=u.A;Cn(t,2===i.c?i.s:Gn(i.z,r.u))}return t}(t.y,r);n=cr(n,t.w);for(var u=t.x,i=0;u.length>i;i++){var f=u[i],a=f.A,o=2===a.c?a.s:Gn(a.z,r.u);n.insertBefore(o,n.childNodes[f.r])}e&&Cn(n,e);return n}(n,r);case 5:return r.s(n);default:q(10)}}function sr(n){if(3===n.nodeType)return qn(n.textContent);if(1!==n.nodeType)return qn("");for(var r=d,t=n.attributes,e=t.length;e--;){var u=t[e];r=h(o(zn,u.name,u.value),r)}var i=n.tagName.toLowerCase(),f=d,a=n.childNodes;for(e=a.length;e--;)f=h(sr(a[e]),f);return c(On,i,r,f)}var br=u(function(n,r,t,e){return yn(r,e,n.aR,n.bc,n.a6,function(r,t){var u=n.be,i=e.node,f=sr(i);return dr(t,function(n){var t=u(n),e=Un(f,t);i=or(i,f,e,r),f=t})})}),lr=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)});function dr(n,r){r(n);var t=0;function e(){t=1===t?0:(lr(e),r(n),1)}return function(u,i){n=u,i?(r(n),2===t&&(t=1)):(0===t&&lr(e),t=2)}}var hr={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var $r,gr=e(function(n,r,t){return{$:0,a:n,b:r,c:t}}),pr=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),mr=1,yr=2,wr=0,Ar=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.d,u=n,i=c(n,t.b,t.c,c(Ar,n,r,t.e));n=u,r=i,t=e}}),Fr=$,_r=function(n){return c(Ar,e(function(n,r,t){return o(Fr,_(n,r),t)}),d,n)},jr=x,kr=e(function(n,r,e){var u=e.c,i=e.d,f=t(function(r,t){return c(jr,r.$?n:f,t,r.a)});return c(jr,f,c(jr,n,r,i),u)}),Er=function(n){return c(kr,Fr,d,n)},Nr=O,Tr=t(function(n,r){return Q(r)/Q(n)}),Lr=Nr(o(Tr,2,32)),Sr=[],xr=v(pr,0,Lr,Sr,Sr),Cr=function(n){return{$:1,a:n}},qr=function(n){return{$:0,a:n}},Br=N,Or=e(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=o(n,t.a,r);n=u,r=i,t=e}}),Rr=function(n){return c(Or,Fr,d,n)},Qr=t(function(n,r){for(;;){var t=o(Br,32,n),e=t.b,u=o(Fr,qr(t.a),r);if(!e.b)return Rr(u);n=e,r=u}}),zr=t(function(n,r){for(;;){var t=Nr(r/32);if(1===t)return o(Br,32,n).a;n=o(Qr,n,d),r=t}}),Ir=R,Yr=t(function(n,r){return w(n,r)>0?n:r}),Dr=function(n){return n.length},Gr=t(function(n,r){if(r.a){var t=32*r.a,e=Ir(o(Tr,32,t-1)),u=n?Rr(r.d):r.d,i=o(zr,u,r.a);return v(pr,Dr(r.c)+t,o(Yr,5,e*Lr),i,r.c)}return v(pr,Dr(r.c),Lr,Sr,r.c)}),Pr=e(function(n,r,t){for(;;){var e=o(Br,32,n),u=e.a,i=e.b;if(0>w(Dr(u),32))return o(Gr,!0,{d:r,a:t,c:u});n=i,r=o(Fr,Cr(u),r),t=t+1}}),Xr=u(function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var f=i.a,a=i.b;if(a.b){var s=a.a,b=a.b;if(b.b){var l=b.b;return o(n,u,o(n,f,o(n,s,o(n,b.a,t>500?c(Or,n,r,Rr(l)):v(Xr,n,r,t+1,l)))))}return o(n,u,o(n,f,o(n,s,r)))}return o(n,u,o(n,f,r))}return o(n,u,r)}return r}),Kr=e(function(n,r,t){return v(Xr,n,r,0,t)}),Mr=t(function(n,r){return r.b?c(Kr,Fr,r,n):n}),Jr=t(function(n,r){return c(Kr,t(function(r,t){return o(Fr,n(r),t)}),d,r)}),Wr=t(function(n,r){return c(Kr,Mr,d,o(Jr,n,r))}),Hr=e(function(n,r,t){for(;;){if(w(n,r)>=1)return t;var e=n,u=r-1,i=o(Fr,r,t);n=e,r=u,t=i}}),Ur=t(function(n,r){return c(Hr,n,r,d)}),Vr=e(function(n,r,t){var e,u=(e=o(Wr,function(r){return o(Jr,function(n){return o(t,n,r)},o(Ur,0,n-1))},o(Ur,0,r-1))).b?c(Pr,e,d,0):xr;return c(gr,n,r,u)}),Zr={$:1},nt=t(function(n){return n}),rt=t(function(n,r){return c(Vr,n,r,nt(nt(Zr)))}),tt=function(n){return!n.$},et=E,ut=i(function(n,r,t,e,u){for(;;){if(0>r)return o(Gr,!1,{d:e,a:t/32|0,c:u});var i=Cr(c(et,32,r,n));n=n,r=r-32,t=t,e=o(Fr,i,e),u=u}}),it=t(function(n,r){if(n>0){var t=n%32;return s(ut,r,n-t-32,n,d,c(et,t,n-t,r))}return xr}),ft=function(n){return{$:0,a:n}},at={$:1},ot=function(n){return{$:1,a:n}},ct=function(n){return{$:0,a:n}},vt=t(function(n,r){return{$:3,a:n,b:r}}),st=t(function(n,r){return{$:0,a:n,b:r}}),bt=t(function(n,r){return{$:1,a:n,b:r}}),lt=function(n){return{$:2,a:n}},dt=function(n){return c(Or,t(function(n,r){return r+1}),0,n)},ht=p,$t=t(function(n,r){return c(ht,n,o(Ur,0,dt(r)-1),r)}),gt=function(n){return n+""},pt=En(d),mt={$:1},yt=En(d),wt=t(function(n,r){return{$:0,a:n,b:r}}),At={$:-2},Ft=At,_t=on,jt=t(function(n,r){return{an:r,au:n}}),kt=_t(o(jt,Ft,Ft)),Et=i(function(n,r,t,e,u){return{$:-1,a:n,b:r,c:t,d:e,e:u}}),Nt=A,Tt=i(function(n,r,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return s(Et,n,r,t,e,u);var i=e.d;v=e.e;return s(Et,0,e.b,e.c,s(Et,1,i.b,i.c,i.d,i.e),s(Et,1,r,t,v,u))}var f=u.b,a=u.c,o=u.d,c=u.e;if(-1!==e.$||e.a)return s(Et,n,f,a,s(Et,0,r,t,e,o),c);var v;return s(Et,0,r,t,s(Et,1,e.b,e.c,e.d,v=e.e),s(Et,1,f,a,o,c))}),Lt=e(function(n,r,t){if(-2===t.$)return s(Et,0,n,r,At,At);var e=t.a,u=t.b,i=t.c,f=t.d,a=t.e;switch(o(Nt,n,u)){case 0:return s(Tt,e,u,i,c(Lt,n,r,f),a);case 1:return s(Et,e,u,r,f,a);default:return s(Tt,e,u,i,f,c(Lt,n,r,a))}}),St=e(function(n,r,t){var e=c(Lt,n,r,t);if(-1!==e.$||e.a)return e;return s(Et,1,e.b,e.c,e.d,e.e)}),xt=e(function(n,r,t){for(;;){if(-2===t.$)return r;var e=t.e,u=n,i=c(n,t.b,t.c,c(xt,n,r,t.d));n=u,r=i,t=e}}),Ct=f(function(n,r,u,i,f,a){var o=c(xt,e(function(t,e,i){n:for(;;){var f=i.a,a=i.b;if(f.b){var o=f.a,s=o.a,b=o.b,l=f.b;if(0>w(s,t)){t=t,e=e,i=_(l,c(n,s,b,a));continue n}return w(s,t)>0?_(f,c(u,t,e,a)):_(l,v(r,s,b,e,a))}return _(f,c(u,t,e,a))}}),_(_r(i),a),f),s=o.a,b=o.b;return c(Or,t(function(r,t){return c(n,r.a,r.b,t)}),b,s)}),qt=function(n){return cn(function(r){var t=n.f;2===t.$&&t.c&&t.c(),n.f=null,r(on(F))})},Bt=vn,Ot=t(function(n,r){n:for(;;){if(-2===r.$)return at;var t=r.c,e=r.d,u=r.e;switch(o(Nt,n,r.b)){case 0:n=n,r=e;continue n;case 1:return ft(t);default:n=n,r=u;continue n}}}),Rt=t(function(n,r){var t=n.a,e=n.b,u=o(Ot,t,r);return c(St,t,1===u.$?g([e]):o(Fr,e,u.a),r)}),Qt=jn,zt=ln,It=Ln,Yt=e(function(n,r,t){if(r.b){var e=r.a,u=r.b,i=zt(o(It,e,o(Qt,n,e)));return o(Bt,function(r){return c(Yt,n,u,c(St,e,r,t))},i)}return _t(t)}),Dt=e(function(n,r,t){var i=t.an,f=e(function(n,r,t){var e=t.c;return j(t.a,t.b,o(Bt,function(){return e},qt(r)))}),a=c(Or,Rt,Ft,r),v=b(Ct,e(function(n,r,t){var e=t.b,u=t.c;return j(o(Fr,n,t.a),e,u)}),u(function(n,r,t,e){var u=e.c;return j(e.a,c(St,n,t,e.b),u)}),f,a,i,j(d,Ft,_t(0))),s=v.a,l=v.b;return o(Bt,function(n){return _t(o(jt,a,n))},o(Bt,function(){return c(Yt,n,s,l)},v.c))}),Gt=_n,Pt=e(function(n,r,t){return o(Bt,function(r){return o(Bt,function(t){return _t(o(n,r,t))},t)},r)}),Xt=function(n){return c(Kr,Pt(Fr),_t(d),n)},Kt=function(n){return n},Mt=($r=Kt,cn(function(n){n(on($r(Date.now())))})),Jt=e(function(n,r,t){var e=o(Ot,r,t.au);if(1===e.$)return _t(t);var u=e.a;return o(Bt,function(){return _t(t)},o(Bt,function(r){return Xt(o(Jr,function(t){return o(Gt,n,t(r))},u))},Mt))}),Wt=e(function(n,r,t){return n(r(t))});wn.Time=An(kt,Dt,Jt,0,t(function(n,r){return o(wt,r.a,o(Wt,n,r.b))}));var Ht=kn("Time"),Ut=t(function(n,r){return Ht(o(wt,n,r))}),Vt=function(n){return n.b},Zt=u(function(n,r,t,e){return n>=0&&0>w(n,t)&&r>=0&&0>w(r,e)?r*t+n:-1}),ne=4294967295>>>32-Lr,re=T,te=L,ee=u(function(n,r,t,e){var u=ne&r>>>n,i=o(re,u,e);return c(te,u,i.$?Cr(c(te,ne&r,t,i.a)):qr(v(ee,n-Lr,r,t,i.a)),e)}),ue=function(n){return n>>>5<<5},ie=e(function(n,r,t){var e=t.a,u=t.b,i=t.c,f=t.d;return 0>n||w(n,e)>-1?t:w(n,ue(e))>-1?v(pr,e,u,i,c(te,ne&n,r,f)):v(pr,e,u,v(ee,u,n,r,i),f)}),fe=u(function(n,r,t,e){var u=e.a,i=e.b,f=e.c;return c(gr,u,i,c(ie,v(Zt,n,r,u,i),t,f))}),ae=function(n){return n.a},oe=e(function(n,r,t){for(;;){var e=o(re,ne&r>>>n,t);if(e.$)return o(re,ne&r,e.a);n=n-Lr,r=r,t=e.a}}),ce=t(function(n,r){var t=r.a,e=r.b,u=r.c,i=r.d;return 0>n||w(n,t)>-1?at:w(n,ue(t))>-1?ft(o(re,ne&n,i)):ft(c(oe,e,n,u))}),ve=e(function(n,r,t){var e=t.c;return o(ce,v(Zt,n,r,t.a,t.b),e)}),se=function(n){return{$:0,a:n}},be=e(function(n,r,t){var e=c(ve,n,r,t.e);if(e.$||1!==e.a.$)return t;return k(t,{e:v(fe,n,r,se(0),t.e)})}),le=S,de=C,he=t(function(n,r){var e=r.c,u=r.d,i={d:d,a:0,c:c(de,n,ue(r.a),u)},f=t(function(r,t){if(r.$){var e=Cr(c(de,n,32*t.a,r.a));return{d:o(Fr,e,t.d),a:t.a+1,c:t.c}}return c(le,f,t,r.a)});return o(Gr,!0,c(le,f,i,e))}),$e=B,ge=t(function(n,r){var t=r.a;return c(gr,t,r.b,o(he,function(r){return function(e){return c(n,o($e,t,r),r/t|0,e)}},r.c))}),pe=t(function(n,r){return c(Kr,t(function(r,t){return n(r)?o(Fr,r,t):t}),d,r)}),me=t(function(n,r){return r.$?n:r.a}),ye=e(function(n,r,t){return k(t,{e:v(fe,n,r,Zr,t.e)})}),we=t(function(n,r){return _(function(){switch(n.$){case 0:return r;case 1:return k(r,{e:(b=r.e,l=t(function(n,r){return g([_(n-1,r-1),_(n,r-1),_(n+1,r-1),_(n-1,r),_(n+1,r),_(n-1,r+1),_(n,r+1),_(n+1,r+1)])}),d=e(function(n,r,t){var e=o($e,ae(t),n),u=o($e,Vt(t),r);return c(ve,e,u,t)}),o(ge,function(n){return function(r){return function(t){var e=se(1===t.$?0:7===t.a?7:t.a+1),u=dt(o(pe,function(n){return!m(n,Zr)},o(Jr,function(n){return o(me,Zr,c(d,n.a,n.b,b))},o(l,n,r))));switch(u){case 0:case 1:return Zr;case 2:return m(t,Zr)?Zr:e;case 3:return e;default:return Zr}}}},b))});case 2:var u=n.c;return k(r,{e:v(fe,i=n.a,f=n.b,function(n){return n.$?se(0):Zr}(u),r.e),A:u.$?1:2});case 3:return k(r,{A:0});case 4:var i=n.a,f=n.b;switch(r.A){case 0:return r;case 1:return c(be,i,f,r);default:return c(ye,i,f,r)}case 5:return k(r,{s:!r.s});default:var a=ae(r.e),s=Vt(r.e);return k(r,{e:o(rt,a,s)})}var b,l,d}(),pt)}),Ae={$:3},Fe={$:6},_e={$:5},je=e(function(n,r,t){return{$:2,a:n,b:r,c:t}}),ke=t(function(n,r){return{$:4,a:n,b:r}}),Ee=K,Ne=M,Te=function(n){return{$:0,a:n}},Le=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Se=Bn("http://www.w3.org/2000/svg"),xe=Se("circle"),Ce=zn("cx"),qe=zn("cy"),Be=zn("fill"),Oe=zn("r"),Re=zn("stroke"),Qe={Q:!0,S:!1},ze=Rn,Ie=t(function(n,r){return o(ze,n,{$:3,a:r})}),Ye=H,De=f(function(n,r,t,e,u,i){return{aB:r,aE:t,aT:n,aW:e,a_:u,a4:i}}),Ge=G,Pe=I,Xe=o(Ee,function(n){switch(n){case 0:return 1;case 1:return 2;case 2:return 3;case 3:return 4;case 4:return 5;default:return 0}},o(Ge,"button",Pe)),Ke=D,Me=c(Ne,t(function(n,r){return _(n,r)}),o(Ge,"clientX",Ke),o(Ge,"clientY",Ke)),Je=Y,We=v(J,e(function(n,r,t){return{ay:n,aG:r,a5:t}}),o(Ge,"altKey",Je),o(Ge,"ctrlKey",Je),o(Ge,"shiftKey",Je)),He=c(Ne,t(function(n,r){return _(n,r)}),o(Ge,"offsetX",Ke),o(Ge,"offsetY",Ke)),Ue=c(Ne,t(function(n,r){return _(n,r)}),o(Ge,"pageX",Ke),o(Ge,"pageY",Ke)),Ve=c(Ne,t(function(n,r){return _(n,r)}),o(Ge,"screenX",Ke),o(Ge,"screenY",Ke)),Ze=l(Ye,De,We,Xe,Me,He,Ue,Ve),nu=e(function(n,r,t){return o(Ie,n,o(Ee,function(n){return{l:t(n),Q:r.Q,S:r.S}},Ze))}),ru=o(nu,"mousedown",Qe),tu=o(nu,"mouseover",Qe),eu=o(nu,"mouseup",Qe),uu={Q:!0,S:!1},iu=W,fu=u(function(n,r,t,e){return{aD:r,aT:n,a7:t,bb:e}}),au=s(iu,u(function(n,r,t,e){return{aE:r,aP:n,a_:t,a4:e}}),o(Ge,"identifier",Pe),Me,Ue,Ve),ou=X,cu=o(Kr,Ne(Fr),Te(d)),vu=function(n){var r=function(r){return o(Ge,gt(r),n)};return o(ou,function(n){return cu(o(Jr,r,o(Ur,0,n-1)))},o(Ge,"length",Pe))},su=s(iu,fu,We,o(Ge,"changedTouches",vu(au)),o(Ge,"targetTouches",vu(au)),o(Ge,"touches",vu(au))),bu=e(function(n,r,t){return o(Ie,n,o(Ee,function(n){return{l:t(n),Q:r.Q,S:r.S}},su))}),lu=o(bu,"touchend",uu),du=o(bu,"touchmove",uu),hu=o(bu,"touchstart",uu),$u=e(function(n,r,t){return o(xe,g([Ce(gt(20*n+10)),qe(gt(20*r+10)),Oe("8"),Re("#888888"),Be(t.$?"#FFFFFF":function(n){switch(n){case 1:return"#FF8800";case 2:return"#FFFF00";case 3:return"#00FF00";case 4:return"#00AA00";case 5:return"#00FFFF";case 6:return"#0000FF";case 7:return"#FF00FF";default:return"#880000"}}(t.a)),ru(function(){return c(je,n,r,t)}),eu(function(){return Ae}),tu(function(){return o(ke,n,r)}),hu(function(){return c(je,n,r,t)}),lu(function(){return Ae}),du(function(){return o(ke,n,r)})]),d)}),gu=function(n){return o(Jr,function(n){return c($u,n.a,n.b,n.c)},(t=(r=n).a,o($t,function(n){return function(r){return j(o($e,t,n),n/t|0,r)}},Er(r.c))));var r,t},pu=On("div"),mu=On("input"),yu=On("label"),wu=On("span"),Au=qn,Fu=fn,_u=t(function(n,r){return o(Qn,n,Fu(r))}),ju=_u("type"),ku=_u("value"),Eu=t(function(n,r){return o(ze,n,{$:0,a:r})}),Nu=function(n){return o(Eu,"click",Te(n))},Tu=Se("svg"),Lu=zn("class"),Su=zn("height"),xu=zn("viewBox"),Cu=zn("width"),qu=_t(0),Bu=t(function(n,r){return o(Bt,function(r){return _t(n(r))},r)}),Ou=t(function(n,r){var t=r;return ln(o(Bt,Gt(n),t))});wn.Task=An(qu,e(function(n,r){return o(Bu,function(){return 0},Xt(o(Jr,Ou(n),r)))}),e(function(){return _t(0)}),t(function(n,r){return o(Bu,n,r)}));kn("Task");var Ru,Qu=br({aR:function(n){return _({s:!1,e:o(rt,n.aw,n._),A:0},pt)},a6:function(n){return n.s?o(Ut,300,function(){return mt}):yt},bc:we,be:function(n){var r=n.e,t=gt(20*ae(r)),e=gt(20*Vt(r));return o(pu,d,g([o(pu,g([eu(function(){return Ae}),lu(function(){return Ae})]),g([o(Tu,g([Cu(t),Su(e),xu("0 0 "+t+" "+e),Lu("life")]),gu(r))])),o(yu,d,g([o(mu,g([ju("checkbox"),Nu(_e)]),d),o(wu,d,g([Au("auto")]))])),o(mu,g([ju("button"),ku("step"),Nu(mt)]),d),o(mu,g([ju("button"),ku("reset"),Nu(Fe)]),d)]))}});Ru={LifeGame:{init:Qu(o(ou,function(n){return o(ou,function(r){return Te({_:r,aw:n})},o(Ge,"height",Pe))},o(Ge,"width",Pe)))(0)}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?q(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,Ru):n.Elm=Ru}(this);