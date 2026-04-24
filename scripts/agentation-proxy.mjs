import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const publicPort = Number.parseInt(process.env.TLON_WEB_PUBLIC_PORT || '8080', 10);
const backendPort = Number.parseInt(process.env.TLON_WEB_BACKEND_PORT || '8081', 10);
const agentationEndpoint = process.env.AGENTATION_ENDPOINT || 'http://localhost:4747';
const repoRoot = process.cwd();
const agentationBundlePath = path.join(repoRoot, 'static', 'agentation.bundle.js');

const injection = [
  `<div id="agentation-root" data-endpoint="${escapeHtml(agentationEndpoint)}"></div>`,
  '<script src="/static/agentation.bundle.js"></script>',
].join('');

const server = http.createServer((clientReq, clientRes) => {
  if (clientReq.url === '/static/agentation.bundle.js') {
    serveAgentationBundle(clientRes);
    return;
  }

  const backendReq = http.request(
    {
      hostname: '127.0.0.1',
      port: backendPort,
      method: clientReq.method,
      path: clientReq.url,
      headers: { ...clientReq.headers, host: `127.0.0.1:${backendPort}` },
    },
    (backendRes) => {
      const chunks = [];
      backendRes.on('data', (chunk) => chunks.push(chunk));
      backendRes.on('end', () => {
        const body = Buffer.concat(chunks);
        const contentType = backendRes.headers['content-type'] || '';

        if (contentType.includes('text/html')) {
          const html = body.toString('utf8');
          const injectedHtml = html.includes('</body>')
            ? html.replace('</body>', `${injection}</body>`)
            : `${html}${injection}`;
          const headers = { ...backendRes.headers };
          delete headers['content-length'];
          clientRes.writeHead(backendRes.statusCode || 200, headers);
          clientRes.end(injectedHtml);
          return;
        }

        clientRes.writeHead(backendRes.statusCode || 200, backendRes.headers);
        clientRes.end(body);
      });
    }
  );

  backendReq.on('error', (error) => {
    clientRes.writeHead(502, { 'content-type': 'text/plain; charset=utf-8' });
    clientRes.end(`Tlon backend on port ${backendPort} is unavailable: ${error.message}\n`);
  });

  clientReq.pipe(backendReq);
});

server.listen(publicPort, '127.0.0.1', () => {
  console.log(`Agentation proxy listening on http://127.0.0.1:${publicPort}`);
  console.log(`Proxying Tlon backend on http://127.0.0.1:${backendPort}`);
});

function serveAgentationBundle(res) {
  fs.readFile(agentationBundlePath, (error, bundle) => {
    if (error) {
      res.writeHead(404, { 'content-type': 'text/plain; charset=utf-8' });
      res.end('agentation.bundle.js not found; run scripts/build-agentation.sh first\n');
      return;
    }

    res.writeHead(200, {
      'content-type': 'text/javascript; charset=utf-8',
      'cache-control': 'no-store',
      'content-length': bundle.length,
    });
    res.end(bundle);
  });
}

function escapeHtml(value) {
  return value
    .replaceAll('&', '&amp;')
    .replaceAll('"', '&quot;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;');
}
