
#pragma once

#include <bond/comm/detail/logging.h>
#include <bond/comm/services.h>
#include <bond/comm/layers_dispatcher.h>

namespace bond { namespace comm
	{
		template <typename Data, typename... Layers>
		class LayerStack;


		template <typename Data,
			typename Layer>
			class LayerStack<Data, Layer>
			: public Layer
		{
		public:

			LayerStack()
			{}


			explicit
				LayerStack(const Layer& layer)
				: Layer(layer)
			{}


			void OnSend(MessageType message_type,
				const std::string& service_name,
				const std::string& method_name,
				Data& data)
			{
				Layer::OnSend(message_type, service_name, method_name, data);
			}


			void OnReceive(MessageType message_type,
				const std::string& service_name,
				const std::string& method_name,
				Data& data)
			{
				Layer::OnReceive(message_type, service_name, method_name, data);
			}
		};


		template <typename Data,
			typename Layer,
			typename... Layers>
			class LayerStack<Data, Layer, Layers...>
			: public LayerStack<Data, Layers...>
			, public Layer
		{
		public:

			LayerStack()
			{}


			explicit
				LayerStack(const Layer& layer, const Layers&... layers)
				: LayerStack<Data, Layers...>(layers...)
				, Layer(layer)
			{}


			void OnSend(MessageType message_type,
				const std::string& service_name,
				const std::string& method_name,
				Data& data)
			{
				Layer::OnSend(message_type, service_name, method_name, data);
				LayerStack<Data, Layers...>::OnSend(message_type, service_name, method_name, data);
			}


			void OnReceive(MessageType message_type,
				const std::string& service_name,
				const std::string& method_name,
				Data& data)
			{
				LayerStack<Data, Layers...>::OnReceive(message_type, service_name, method_name, data);
				Layer::OnReceive(message_type, service_name, method_name, data);
			}
		};


		//
		// Layer extension for transports.
		//
		template <typename BaseTransport,
			typename LayerProtocol>
			class LayerTransport
			: public BaseTransport
			, public LayerProtocol
		{
		protected:

			//
			// Class utility for spliting implementation of methods LayerTransport::ConnectTo and LayerTransport::BindTo
			// Different behavior for transports inherited from LayerProvider and thereafter providing support of extended methods of ILayerService
			//

			// Not-inherited from LayerProvider. Uses original implementation of this methods
			template <typename Transport, bool support_layers = std::is_base_of<LayerProvider<Address>, Transport>::value >
			struct LayerServiceType {

				template<typename Address, typename WireProtocol>
				static inline
					boost::shared_ptr<IService>
					ConnectTo(const Address& address
						, std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)> &factory
						, const WireProtocol& protocol, LayerTransport<BaseTransport, LayerProtocol> *transport
						, boost::shared_ptr<IService>(LayerTransport<BaseTransport, LayerProtocol>::*mthd)(const Address &))
				{
					return factory(true, protocol, address, (transport->*mthd)(address));
				}


				template<typename Address, typename WireProtocol, typename ConnectionContext>
				static inline
					boost::shared_ptr<void>
					BindTo(const Address& address,
						const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>& tableFactory
						, std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)> &factory
						, const WireProtocol& protocol, LayerTransport<BaseTransport, LayerProtocol> *transport
						, boost::shared_ptr<void>(LayerTransport<BaseTransport, LayerProtocol>::*mthd)(const Address &, const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>&))
				{
					return (transport->*mthd)(
						address,
						[tableFactory, protocol, factory](const Address& remoteAddress, ConnectionContext context) {
						return factory(false, protocol, remoteAddress, tableFactory(remoteAddress, context));
					});
				}
			};

			// Inherited from LayerProvider. Uses original implementation of this methods
			template <typename Transport>
			struct LayerServiceType<Transport, true> {

				template<typename Address, typename WireProtocol >
				static inline
					boost::shared_ptr<IService>
					ConnectTo(const Address& address
						, std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)> &factory
						, const WireProtocol& protocol, LayerTransport<BaseTransport, LayerProtocol> *transport
						, boost::shared_ptr<IService>(LayerTransport<BaseTransport, LayerProtocol>::*mthd)(const Address &))
				{
					// provides ILayerTransport::_factory to client part of transport object
					transport->SetLayerServiceClientFunc( [protocol, factory](const Address& remoteAddress, const boost::shared_ptr<IService>& serv) {
						return factory(true, protocol, remoteAddress, serv);
					});
					return (transport->*mthd)(address);
				}

				template<typename Address, typename WireProtocol, typename ConnectionContext>
				static inline
					boost::shared_ptr<void>
					BindTo(const Address& address,
						const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>& tableFactory
						, std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)> &factory
						, const WireProtocol& protocol, LayerTransport<BaseTransport, LayerProtocol> *transport
						, boost::shared_ptr<void>(LayerTransport<BaseTransport, LayerProtocol>::*mthd)(const Address &, const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>&))
				{
					// provides ILayerTransport::_factory to server part of transport object
					transport->SetLayerServiceServerFunc([protocol, factory](const Address& remoteAddress, const boost::shared_ptr<IService>& serv) {
						return factory(false, protocol, remoteAddress, serv);
					});
					return (transport->*mthd)(address, [tableFactory](const Address& remoteAddress, ConnectionContext context) {
						return tableFactory(remoteAddress, context);
					});
				}
			};

		public:

			typedef typename BaseTransport::WireProtocol WireProtocol;
			typedef typename BaseTransport::ConnectionContext ConnectionContext;
			typedef typename BaseTransport::Address Address;


			template <typename... Args>
			explicit
				LayerTransport(Args&&... args)
				: BaseTransport(std::forward<Args>(args)...)
				, _factory(CreateNullFactory())
			{}


			template <typename Data, typename... Layers, typename... Args>
			explicit
				LayerTransport(LayerStack<Data, Layers...> layerStack,
					Args&&... args)
				: BaseTransport(std::forward<Args>(args)...)
				, _factory(CreateFactory(std::move(layerStack)))
			{}

		protected:

			boost::shared_ptr<IService>
				ConnectToImpl(const Address& address)
			{
				return BaseTransport::ConnectTo(address);
			}

			boost::shared_ptr<IService>
				ConnectTo(const Address& address) override
			{
				// Determines the LayerProvider inheritance of current transport
				return LayerServiceType<BaseTransport>::ConnectTo(address, _factory, static_cast<const WireProtocol &>(*this), this, &LayerTransport::ConnectToImpl);
			}


			boost::shared_ptr<void>
				BindToImpl(const Address& address,
					const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>& tableFactory)
			{
				return BaseTransport::BindTo(address, tableFactory);
			}

			boost::shared_ptr<void>
				BindTo(const Address& address,
					const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>& tableFactory) override
			{
				// Determines the LayerProvider inheritance of current transport
				return LayerServiceType<BaseTransport>::BindTo(address, tableFactory, _factory
					, static_cast<const WireProtocol &>(*this), this, &LayerTransport::BindToImpl);
			}

		private:

			static
				std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)>
				CreateNullFactory()
			{
				return [](bool, const WireProtocol&, const Address&, const boost::shared_ptr<IService>& service) {
					return boost::make_shared<LayerServiceStub>(service);
				};
			}
			template <typename Data, typename... Layers>
			static
				std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)>
				CreateFactory(const LayerStack<Data, Layers...>& layerStack)
			{
				return
					[layerStack](bool onConnect, const WireProtocol& protocol, const Address& address, const boost::shared_ptr<IService>& service) {
					return onConnect
						? boost::shared_ptr<ILayerService>(boost::make_shared<Processor<Outgoing, Data, LayerStack<Data, Layers...>, ILayerService>>(address, service, layerStack, protocol))
						: boost::shared_ptr<ILayerService>(boost::make_shared<Processor<Incoming, Data, LayerStack<Data, Layers...>, ILayerService>>(address, service, layerStack, protocol));
				};
			}
		protected:

			// ctor receiving initialized factory from child class
			template <typename... Args>
			explicit
				LayerTransport(std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)> __factory, Args&&... args)
				: BaseTransport(std::forward<Args>(args)...)
				, _factory(__factory)
			{}

			// typename ServiceType - needed for inheritance redirection in v2::TransportLauer::Processor
			template <typename Policy, typename LayerData, typename LayerStack, typename ServiceType = IService>
			struct Processor
				: public ServiceType
				, public LayerStack
				, public Policy
			{
				Processor(const Address& address,
					const boost::shared_ptr<IService>& service,
					const LayerStack& layerStack,
					const WireProtocol& protocol)
					: LayerStack(layerStack)
					, Policy(protocol)
					, _service(service)
					, _address(address)
				{}


				void Invoke(Request& request, const ResponseCallback& callback) override
				{
					LayerStack layerStack(static_cast<const LayerStack&>(*this));

					LayerData data;
					Policy policy = *this;
					policy.OnRequest(layerStack,
						MessageType::REQUEST,
						request.service_name,
						request.method_name,
						data,
						request.layers);

					if (auto serv = _service.lock())
					{
						serv->Invoke(
							request,
							[policy, layerStack, callback](Response& response) mutable {
							LayerData data;
							policy.OnResponse(layerStack,
								data,
								response.layers);

							callback(response);
						});
					}
				}


				void Notify(Event& event) override
				{
					LayerStack layerStack(static_cast<const LayerStack&>(*this));

					LayerData data;
					Policy policy = *this;
					policy.OnRequest(layerStack,
						MessageType::EVENT,
						event.service_name,
						event.method_name,
						data,
						event.layers);

					if (auto serv = _service.lock())
						serv->Notify(event);
				}


				void OnConnStateChanged(ILayerService::EConnectionStatus, bool &) override {}


				boost::weak_ptr<IService> _service;

				Address _address;
			};


			struct Incoming : WireProtocol
			{
				Incoming(const WireProtocol& protocol)
					: WireProtocol(protocol)
				{}


				template <typename LayerData, typename LayerStack>
				void OnRequest(LayerStack& layerStack,
					MessageType type,
					const std::string& service_name,
					const std::string& method_name,
					LayerData& data,
					std::vector<blob>& buffers)
				{
					_service_name = service_name;
					_method_name = method_name;

					if (buffers.empty())
					{
						BOND_LOG(LOG_WARNING,
							"LayerTransport",
							"Layer stack present but no layer data received.");
					}
					else
					{
						WireProtocol::template Unpack<LayerData>(buffers).Deserialize(data);
					}

					layerStack.OnReceive(type, service_name, method_name, data);
				}


				template <typename LayerData, typename LayerStack>
				void OnResponse(LayerStack& layerStack,
					LayerData& data,
					std::vector<blob>& buffers)
				{
					layerStack.OnSend(MessageType::RESPONSE, _service_name, _method_name, data);

					buffers = WireProtocol::Pack(bonded<LayerData>(data));
				}

				std::string _service_name;
				std::string _method_name;
			};


			struct Outgoing : WireProtocol
			{
				Outgoing(const WireProtocol& protocol)
					: WireProtocol(protocol)
				{}


				template <typename LayerData, typename LayerStack>
				void OnRequest(LayerStack& layerStack,
					MessageType type,
					const std::string& service_name,
					const std::string& method_name,
					LayerData& data,
					std::vector<blob>& buffers)
				{
					_service_name = service_name;
					_method_name = method_name;

					layerStack.OnSend(type, service_name, method_name, data);

					buffers = WireProtocol::Pack(bonded<LayerData>(data));
				}


				template <typename LayerData, typename LayerStack>
				void OnResponse(LayerStack& layerStack,
					LayerData& data,
					std::vector<blob>& buffers)
				{
					if (buffers.empty())
					{
						BOND_LOG(LOG_WARNING,
							"LayerTransport",
							"Layer stack present but no layer data received.");
					}
					else
					{
						WireProtocol::template Unpack<LayerData>(buffers).Deserialize(data);
					}

					layerStack.OnReceive(MessageType::RESPONSE, _service_name, _method_name, data);
				}

				std::string _service_name;
				std::string _method_name;
			};


			std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)> _factory;
		}; // class LayerTransport

		// Additional namespace added to maintain compatibility with previous versions of tests / examples / software
		namespace v2
		{

			template <typename Data, typename... Layers>
			class LayerStack;

			// helper utility class checking availability of a method call
			template<bool>
			struct is_required_class
			{
				template< typename MethodClass, typename Object, typename... TArgs >
				inline static void DoCall(void(MethodClass::*)(TArgs...), Object &/*obj*/, TArgs&&... /*args*/)
				{
				}
			};
			template<>
			struct is_required_class< true >
			{
				template< typename MethodClass, typename Object, typename... TArgs >
				inline static void DoCall(void(MethodClass::*mthd)(TArgs...), Object &obj, TArgs&&... args)
				{
					(obj.*mthd)(args...);
				}
			};

			// implementation of LayerStack independent of Layer object structure
			template <typename Data,
				typename Layer>
				class LayerStack<Data, Layer>
				: public Layer
			{
			public:

				LayerStack()
				{}


				explicit
					LayerStack(const Layer& layer)
					: Layer(layer)
				{}

				// makes a call of Layer's method if exists
				template< typename RequiredType, typename... TArgs >
				void Unwind(void(RequiredType::*mthd)(TArgs...), bool /*unwind_first*/, TArgs&&... args)
				{
					is_required_class < std::is_base_of< RequiredType, Layer >::value >::DoCall<RequiredType, Layer>(mthd, *(static_cast<Layer *>(this)), std::forward<TArgs>(args)...);
				}
			};


			template <typename Data,
				typename Layer,
				typename... Layers>
				class LayerStack<Data, Layer, Layers...>
				: public LayerStack<Data, Layers...>
				, public Layer
			{
			public:

				LayerStack()
				{}


				explicit
					LayerStack(const Layer& layer, const Layers&... layers)
					: LayerStack<Data, Layers...>(layers...)
					, Layer(layer)
				{}

				// makes a call of Layer's method if exists and delegates it to higher Layer
				// unwind_first - indicates the sequence the order of call/delegate
				template< typename RequiredType, typename... TArgs >
				void Unwind(void(RequiredType::*mthd)(TArgs...), bool unwind_first, TArgs&&... args)
				{
					if (unwind_first)
					{
						LayerStack<Data, Layers...>::Unwind(mthd, unwind_first, std::forward<TArgs>(args)...);
						is_required_class < std::is_base_of< RequiredType, Layer >::value >::DoCall(mthd, *(static_cast<Layer *>(this)), std::forward<TArgs>(args)...);
					}
					else
					{
						is_required_class < std::is_base_of< RequiredType, Layer >::value >::DoCall(mthd, *(static_cast<Layer *>(this)), std::forward<TArgs>(args)...);
						LayerStack<Data, Layers...>::Unwind(mthd, unwind_first, std::forward<TArgs>(args)...);
					}
				}

			};

			// implementation of a child of bond::comm::LayerTransport
			template <typename BaseTransport,
				typename LayerProtocol>
				class LayerTransport
				: public bond::comm::LayerTransport< BaseTransport, LayerProtocol >
			{
				typedef bond::comm::LayerTransport< BaseTransport, LayerProtocol > Base;
			public:

				template <typename... Args>
				explicit
					LayerTransport(Args&&... args)
					: Base(args...)
				{}


				template <typename Data, typename... Layers, typename... Args>
				explicit
					LayerTransport(LayerStack<Data, Layers...> layerStack,
						Args&&... args)
					: Base(CreateFactory(std::move(layerStack)), args...)
				{}

			private:

				template <typename Data, typename... Layers>
				static
					std::function<boost::shared_ptr<ILayerService>(bool onConnect, const WireProtocol&, const Address&, const boost::shared_ptr<IService>&)>
					CreateFactory(const LayerStack<Data, Layers...>& layerStack)
				{
					return
						[layerStack](bool onConnect, const WireProtocol& protocol, const Address& address, const boost::shared_ptr<IService>& service) {
						return onConnect
							? boost::shared_ptr<ILayerService>(boost::make_shared<Processor<Outgoing, Data, LayerStack<Data, Layers...>>>(address, service, layerStack, protocol))
							: boost::shared_ptr<ILayerService>(boost::make_shared<Processor<Incoming, Data, LayerStack<Data, Layers...>>>(address, service, layerStack, protocol));
					};
				}

				// implementation of a child of bond::comm::LayerTransport::Processor
				// added for future use: adding new injection points
				template <typename Policy, typename LayerData, typename LayerStack>
				struct Processor : public bond::comm::LayerTransport<BaseTransport, LayerProtocol>::Processor<Policy, LayerData, LayerStack, ILayerService>
				{
					Processor(const Address& address,
						const boost::shared_ptr<IService>& service,
						const LayerStack& layerStack,
						const WireProtocol& protocol)
						: bond::comm::LayerTransport<BaseTransport, LayerProtocol>::Processor<Policy, LayerData, LayerStack, ILayerService>( address, service, layerStack, protocol)
					{}

					void OnConnStateChanged(ILayerService::EConnectionStatus status, bool &reconnect) override
					{
						LayerStack layerStack(static_cast<const LayerStack&>(*this));
						layerStack.Unwind< OnConnStateChangedLayerService >(&OnConnStateChangedLayerService::OnConnStateChanged, true, std::forward<ILayerService::EConnectionStatus>(status), reconnect);
					}

					~Processor()
					{}
				};


				struct Incoming : WireProtocol
				{
					Incoming(const WireProtocol& protocol)
						: WireProtocol(protocol)
					{}


					template <typename LayerData, typename LayerStack>
					void OnRequest(LayerStack& layerStack,
						MessageType type,
						const std::string& service_name,
						const std::string& method_name,
						LayerData& data,
						std::vector<blob>& buffers)
					{
						_service_name = service_name;
						_method_name = method_name;

						if (buffers.empty())
						{
							BOND_LOG(LOG_WARNING,
								"LayerTransport",
								"Layer stack present but no layer data received.");
						}
						else
						{
							WireProtocol::template Unpack<LayerData>(buffers).Deserialize(data);
						}

						// unwinds layers stack passing specified method
						layerStack.Unwind< OnReceiveLayerService< LayerData > >(&OnReceiveLayerService< LayerData >::OnReceive, true, std::forward< MessageType>(type), service_name, method_name, data);
					}


					template <typename LayerData, typename LayerStack>
					void OnResponse(LayerStack& layerStack,
						LayerData& data,
						std::vector<blob>& buffers)
					{
						layerStack.Unwind< OnSendLayerService< LayerData > >(&OnSendLayerService< LayerData >::OnSend, false, std::forward< MessageType>(MessageType::RESPONSE), std::forward< const std::string &>(_service_name), std::forward< const std::string &>(_method_name), data);

						// unwinds layers stack passing specified method
						buffers = WireProtocol::Pack(bonded<LayerData>(data));
					}

					std::string _service_name;
					std::string _method_name;
				};


				struct Outgoing : WireProtocol
				{
					Outgoing(const WireProtocol& protocol)
						: WireProtocol(protocol)
					{}


					template <typename LayerData, typename LayerStack>
					void OnRequest(LayerStack& layerStack,
						MessageType type,
						const std::string& service_name,
						const std::string& method_name,
						LayerData& data,
						std::vector<blob>& buffers)
					{
						_service_name = service_name;
						_method_name = method_name;

						// unwinds layers stack passing specified method
						layerStack.Unwind< OnSendLayerService< LayerData > >(&OnSendLayerService< LayerData >::OnSend, false, std::forward< MessageType>(type), service_name, method_name, data);

						buffers = WireProtocol::Pack(bonded<LayerData>(data));
					}


					template <typename LayerData, typename LayerStack>
					void OnResponse(LayerStack& layerStack,
						LayerData& data,
						std::vector<blob>& buffers)
					{
						if (buffers.empty())
						{
							BOND_LOG(LOG_WARNING,
								"LayerTransport",
								"Layer stack present but no layer data received.");
						}
						else
						{
							WireProtocol::template Unpack<LayerData>(buffers).Deserialize(data);
						}

						// unwinds layers stack passing specified method
						layerStack.Unwind< OnReceiveLayerService< LayerData > >(&OnReceiveLayerService< LayerData >::OnReceive, true, std::forward< MessageType>(MessageType::RESPONSE), std::forward< const std::string &>(_service_name), std::forward< const std::string &>(_method_name), data);
					}

					std::string _service_name;
					std::string _method_name;
				};

			}; // class LayerTransport
		} // namespace v2

} } // namespace bond.comm
