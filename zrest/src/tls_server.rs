use std::io;
use std::fmt::Display;
use hyper::{
    service::{Service, NewService},
    server::conn::Http,
    rt,
    body::Payload,
    Body
};
use native_tls::{Identity, TlsAcceptor};
use tokio_tls::TlsAcceptor as TokioTlsAcceptor;
use tokio_tcp::TcpListener;
use futures::{Future, Stream};

use error::*;

pub struct TlsServer;

impl TlsServer {
    pub fn create<NS, B>(srv: TcpListener, tls_cx: TlsAcceptor, new_service: NS) -> impl Future<Item=(), Error=()> + Send
        where NS: NewService<ReqBody = Body, ResBody=B> + Send + 'static,
              <NS::Service as Service>::Future: Send + 'static,
              <NS as NewService>::Future: Send,
              <NS as NewService>::Service: Send,
              <NS as NewService>::InitError: Display,
              B: Payload
    {
        let tls_cx = TokioTlsAcceptor::from(tls_cx);
        let http_proto = Http::new();
        let http_server = http_proto
            .serve_incoming(
                srv.incoming().and_then(move |socket| {
                    tls_cx
                        .accept(socket)
                        .map_err(|e| io::Error::new(io::ErrorKind::Other, e))
                }),
                new_service,
            )
            .then(|res| {
                match res {
                    Ok(conn) => Ok(Some(conn)),
                    Err(e) => {
                        error!("Error establishing TLS connection: {}", e);
                        Ok(None)
                    },
                }
            })
            .for_each(|conn_opt| {
                if let Some(conn) = conn_opt {
                    rt::spawn(
                        conn
                            .map_err(|e| error!("Connection error {}", e))
                            .and_then(|c| c.with_upgrades().map_err(|e| error!("Hyper error {}", e)))
                    );
                }

                Ok(())
            });
        http_server
    }
}

pub fn create_tls_cx(pem_der: &[u8], password: &str) -> Result<TlsAcceptor, ZError> {
    let cert = Identity::from_pkcs12(pem_der, password)
        .map_err(|e| Into::<ZError>::into(e))?;
    TlsAcceptor::builder(cert).build()
        .map_err(|e| e.into())
}